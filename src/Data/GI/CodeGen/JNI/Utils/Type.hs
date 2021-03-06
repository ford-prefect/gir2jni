{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.GI.CodeGen.JNI.Utils.Type where

import Control.Applicative ((<|>))
import Control.Monad (join)
import qualified Data.Map as M (lookup, union)
import Data.String (fromString)
import qualified Data.Text as T (toLower, unpack)

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

import qualified Language.Java.Syntax as JSyn

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import Data.GI.CodeGen.JNI.Types

giNamespaceToJava :: Package -> GI.Name -> Package
giNamespaceToJava pkg giName = pkg ++ [T.unpack . T.toLower . GI.namespace $ giName]

javaClassType :: [String] -> JSyn.ClassType
javaClassType n = JSyn.ClassType . fmap (,[]) $ JSyn.Ident <$> n

javaClassRef :: [String] -> JSyn.RefType
javaClassRef = JSyn.ClassRefType . javaClassType

javaStringType :: JSyn.Type
javaStringType = JSyn.RefType . javaClassRef $  ["java", "lang", "String"]

javaFQToClassRef :: FQClass -> JSyn.RefType
javaFQToClassRef (pkg, cls) = javaClassRef (pkg ++ [cls])

giNameToType :: GI.Name -> GIType.Type
giNameToType (GI.Name ns n) = GIType.TInterface ns n

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

giIsStringType :: GIType.Type -> Bool
giIsStringType (GIType.TBasicType t) = giBasicTypeToJava t == javaStringType
giIsStringType _                     = False

giIsGObject :: GI.Name -> Bool
giIsGObject (GI.Name "GObject" "Object") = True
giIsGObject _                            = False

giIsObjectType :: Info -> GIType.Type -> Bool
giIsObjectType Info{..} (GIType.TInterface ns n) =
  let
    name = GI.Name ns n
    apis = M.union infoAPI infoDeps
    api  = M.lookup name apis
  in
    case api of
      Just (GI.APIObject GI.Object{..}) -> isGObjectType apis name objParent
      _                                 -> False
  where

    isGObjectType apis _ (Just p) = isGObjectType apis p (join . fmap getObjectParent $ M.lookup p apis)
    isGObjectType apis n Nothing  = giIsGObject n

    getObjectParent (GI.APIObject GI.Object{..}) = objParent
    getObjectParent _                            = Nothing -- This should be an error?
giIsObjectType Info{..} _ = False

giTypeToJNI :: Info -> GIType.Type -> CDSL.CTypeSpec
giTypeToJNI info giType =
  case giType of
    (GIType.TBasicType t)       -> CDSL.ty . fromString . giBasicTypeToJNI $ t
    (GIType.TInterface cls ref) -> if giIsObjectType info giType
                                   then CDSL.ty "jobject"
                                   else CDSL.ty . fromString . giBasicTypeToJNI $ GIType.TLong
    -- FIXME: all the other types
    _                           -> CDSL.ty . fromString . giBasicTypeToJNI $ GIType.TLong
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

giTypeToC :: Info -> GIType.Type -> (CDSL.CTypeSpec, Bool)
giTypeToC info@Info{..} giType =
  let typ   = case giType of
                GIType.TBasicType t       -> CDSL.ty . fromString . giBasicTypeToC $ t
                GIType.TInterface cls ref -> CDSL.ty . fromString $ doLookup (GI.Name cls ref)
                -- FIXME: Deal with other types
                _                         -> CDSL.longSpec
      isPtr = case giType of
                (GIType.TBasicType GIType.TUTF8) -> True
                (GIType.TInterface cls ref     ) -> isInterfacePtrType cls ref
                _                                -> False
  in
    case giType of
      GIType.TError -> giTypeToC info (GIType.TInterface "GLib" "Error")
      _             -> (typ, isPtr)
  where
    doLookup name = case M.lookup name infoCTypes of
      Nothing -> error $ "Don't know C type for GI type: " ++ show name
      Just t  -> T.unpack t

    isInterfacePtrType cls ref =
      let
        name = GI.Name cls ref
        api  = M.lookup name infoAPI <|> M.lookup name infoDeps
      in
        case api of
          Nothing -> error $ "Unknown type reference: " ++ show name
          Just t  -> case t of
            GI.APIInterface _ -> True
            GI.APIObject    _ -> True
            GI.APIStruct    _ -> True
            GI.APIUnion     _ -> True
            _                 -> False

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
      GIType.TPtr      -> "gpointer"
      GIType.TIntPtr   -> "gintptr"
      GIType.TUIntPtr  -> "guintptr"
      GIType.TUTF8     -> "char"
      GIType.TFileName -> "char"

giArgIsOutArg :: GI.Arg -> Bool
giArgIsOutArg GI.Arg{..} =
  -- FIXME: we ignore the out part of inout arguments
  direction == GI.DirectionOut
