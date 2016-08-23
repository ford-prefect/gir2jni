{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Data.GI.CodeGen.JNI.Utils where

import qualified Data.Char as C (toLower)
import Data.List (intercalate)
import qualified Data.Map as M (Map, lookup)
import Data.String (fromString)
import qualified Data.Text as T (Text, toLower, unpack)
import qualified Data.Text.Manipulate as TManip (toCamel)

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

import qualified Language.Java.Syntax as JSyn
import qualified Language.Java.Pretty as JPretty

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import Data.GI.CodeGen.JNI.Types

javaClassRef :: [JSyn.Ident] -> JSyn.Type
javaClassRef = JSyn.RefType . JSyn.ClassRefType . JSyn.ClassType . fmap (,[])

javaStringType :: JSyn.Type
javaStringType = javaClassRef . fmap JSyn.Ident $ ["java", "lang", "String"]

jniTypeDefDecl :: String -> String -> Bool -> Maybe CDSL.CExpr -> CDSL.CDecl
jniTypeDefDecl typ name isPtr =
  let
     typeSpec = CDSL.CTypeSpec . CDSL.ty . fromString $ typ
     doPtr    = if isPtr then CDSL.ptr else id
     ident    = doPtr . fromString $ name
  in
    CDSL.decl typeSpec ident

emptyDecl :: CDSL.CDeclr
emptyDecl = CDSL.CDeclr Nothing [] Nothing [] CDSL.undefNode

makeTypeDecl :: Bool -> CDSL.CDeclr -> CDSL.CTypeSpec -> CDSL.CDecl
makeTypeDecl isPtr ident typ =
  let
    maybePtr = if isPtr then ptr else id
  in
    decl (CDSL.CTypeSpec typ) (maybePtr ident) Nothing

typeDecl :: CDSL.CTypeSpec -> CDSL.CDecl
typeDecl = makeTypeDecl False emptyDecl

typePtrDecl :: CDSL.CTypeSpec -> CDSL.CDecl
typePtrDecl = makeTypeDecl True emptyDecl

jniNull :: CDSL.CExpr
jniNull =
    0 `castTo` typePtrDecl voidSpec

jniEnvArg :: String
jniEnvArg = "env"

jniEnvDecl :: Maybe CDSL.CExpr -> CDSL.CDecl
jniEnvDecl = jniTypeDefDecl "JNIEnv" jniEnvArg True

jniClassDecl :: Maybe CDSL.CExpr -> CDSL.CDecl
jniClassDecl = jniTypeDefDecl "jclass" "clazz" False

-- Java doesn't have unsigned types, so we promote all unsigned types to
-- the next wider Java signed type (except long, where we just have to hope
-- there aren't overflows -- writing bindings is fun.)
giBasicTypeToJava :: GIType.BasicType -> JSyn.Type
giBasicTypeToJava typ =
  case typ of
    GIType.TBoolean  -> JSyn.PrimType JSyn.BooleanT
    GIType.TInt      -> JSyn.PrimType JSyn.IntT
    GIType.TUInt     -> JSyn.PrimType JSyn.LongT
    GIType.TLong     -> JSyn.PrimType JSyn.LongT
    GIType.TULong    -> JSyn.PrimType JSyn.LongT
    GIType.TInt8     -> JSyn.PrimType JSyn.ByteT
    GIType.TUInt8    -> JSyn.PrimType JSyn.IntT
    GIType.TInt16    -> JSyn.PrimType JSyn.ShortT
    GIType.TUInt16   -> JSyn.PrimType JSyn.IntT
    GIType.TInt32    -> JSyn.PrimType JSyn.IntT
    GIType.TUInt32   -> JSyn.PrimType JSyn.LongT
    GIType.TInt64    -> JSyn.PrimType JSyn.LongT
    GIType.TUInt64   -> JSyn.PrimType JSyn.LongT
    GIType.TFloat    -> JSyn.PrimType JSyn.FloatT
    GIType.TDouble   -> JSyn.PrimType JSyn.DoubleT
    GIType.TUniChar  -> JSyn.PrimType JSyn.LongT
    GIType.TGType    -> JSyn.PrimType JSyn.LongT
    GIType.TPtr      -> JSyn.PrimType JSyn.LongT
    GIType.TIntPtr   -> JSyn.PrimType JSyn.LongT
    GIType.TUIntPtr  -> JSyn.PrimType JSyn.LongT
    GIType.TUTF8     -> javaStringType
    GIType.TFileName -> javaStringType

giTypeToJava :: [JSyn.Ident] -> GIType.Type -> JSyn.Type
giTypeToJava prefix giType =
  case giType of
    (GIType.TBasicType t)       -> giBasicTypeToJava t
    -- FIXME: Commenting out until we actually implement generation of classes for objects etc.
    -- (GIType.TInterface cls ref) -> javaClassRef (prefix `mappend` (JSyn.Ident . T.unpack <$> [T.toLower cls, ref]))
    _                           -> JSyn.PrimType JSyn.LongT -- FIXME

giArgToJava :: [JSyn.Ident] -> GI.Arg -> JSyn.FormalParam
giArgToJava prefix giArg =
  let
    var = JSyn.VarId . JSyn.Ident . T.unpack . GI.argCName $ giArg
    typ = giTypeToJava prefix . GI.argType $ giArg
  in
    JSyn.FormalParam [] typ False var

giCVarPrefix :: String
giCVarPrefix = "c_"

giArgToCIdent :: GI.Arg -> String
giArgToCIdent GI.Arg{..} = giCVarPrefix ++ T.unpack argCName

giNamespaceToJava :: Package -> GI.Name -> Package
giNamespaceToJava pkg giName = pkg ++ [T.unpack . T.toLower . GI.namespace $ giName]

giNameToJava :: GI.Name -> String
giNameToJava = T.unpack . toCamelCase . GI.name

giNameToJNI :: Package -> GI.Name -> String
giNameToJNI packagePrefix giName =
  intercalate "_" $ ["Java"]
                  ++ giNamespaceToJava packagePrefix giName
                  ++ [T.unpack . GI.namespace $ giName, giNameToJava giName]

giTypeToJNI :: Maybe GIType.Type -> CDSL.CTypeSpec
giTypeToJNI giType =
  case giType of
    Nothing                      -> CDSL.voidSpec
    (Just (GIType.TBasicType t)) -> CDSL.ty . fromString . giBasicTypeToJNI $ t
    -- FIXME
    -- (GIType.TInterface cls ref) -> undefined
    _                            -> CDSL.ty . fromString . giBasicTypeToJNI $ GIType.TLong
  where
    giBasicTypeToJNI typ = case giBasicTypeToJava typ of
      (JSyn.PrimType JSyn.BooleanT) -> "jboolean"
      (JSyn.PrimType JSyn.ByteT   ) -> "jbyte"
      (JSyn.PrimType JSyn.ShortT  ) -> "jshort"
      (JSyn.PrimType JSyn.IntT    ) -> "jint"
      (JSyn.PrimType JSyn.LongT   ) -> "jlong"
      (JSyn.PrimType JSyn.FloatT  ) -> "jfloat"
      (JSyn.PrimType JSyn.DoubleT ) -> "jdouble"
      javaStringType                -> "jstring"

giTypeToC :: M.Map GI.Name T.Text -> GIType.Type -> (CDSL.CTypeSpec, Bool)
giTypeToC cTypes giType =
  let typ   = case giType of
                GIType.TBasicType t       -> CDSL.ty . fromString . giBasicTypeToC $ t
                GIType.TInterface cls ref -> CDSL.ty . fromString $ doLookup cTypes (GI.Name cls ref)
                -- FIXME: Deal with other types
                _                         -> CDSL.longSpec
      isPtr = case giType of
                (GIType.TBasicType GIType.TUTF8) -> True
                (GIType.TInterface _ _         ) -> True
                _                                -> False
  in
    (typ, isPtr)
  where
    giBasicTypeToC typ = case typ of
      GIType.TBoolean  -> "gboolean"
      GIType.TInt      -> "gint"
      GIType.TUInt     -> "guint"
      GIType.TLong     -> "glong"
      GIType.TULong    -> "gulong"
      GIType.TInt8     -> "gint8"
      GIType.TUInt8    -> "guint8"
      GIType.TInt16    -> "gint16"
      GIType.TUInt16   -> "guint16"
      GIType.TInt32    -> "gint32"
      GIType.TUInt32   -> "guint32"
      GIType.TInt64    -> "gint64"
      GIType.TUInt64   -> "guint64"
      GIType.TFloat    -> "gfloat"
      GIType.TDouble   -> "gdouble"
      GIType.TUniChar  -> "guinchar"
      GIType.TGType    -> "GType"
      GIType.TPtr      -> "gpointr"
      GIType.TIntPtr   -> "gintptr"
      GIType.TUIntPtr  -> "guintptr"
      GIType.TUTF8     -> "char"
      GIType.TFileName -> "char"
    doLookup cTypes name = case M.lookup name cTypes of
      Nothing -> error $ "Don't know C type for GI type: " ++ show name
      Just t  -> T.unpack t

giArgToJNIIdent :: GI.Arg -> String
giArgToJNIIdent GI.Arg{..} = T.unpack argCName

giArgToJNI :: GI.Arg -> (Maybe CDSL.CExpr -> CDSL.CDecl)
giArgToJNI arg@GI.Arg{..} =
  let
    typ  = CDSL.CTypeSpec . giTypeToJNI . Just $ argType
    name = fromString . giArgToJNIIdent $ arg
  in
    CDSL.decl typ name

-- This one uses Text instead of String since that's what GI and text-manipulate are using
toCamelCase :: T.Text -> T.Text
toCamelCase = TManip.toCamel
