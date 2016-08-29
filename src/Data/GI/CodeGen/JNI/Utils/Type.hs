{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.GI.CodeGen.JNI.Utils.Type where

import Control.Applicative ((<|>))
import qualified Data.Map as M (lookup)
import Data.String (fromString)
import qualified Data.Text as T (unpack)

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

import qualified Language.Java.Syntax as JSyn

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import Data.GI.CodeGen.JNI.Types

javaClassRef :: [JSyn.Ident] -> JSyn.Type
javaClassRef = JSyn.RefType . JSyn.ClassRefType . JSyn.ClassType . fmap (,[])

javaStringType :: JSyn.Type
javaStringType = javaClassRef . fmap JSyn.Ident $ ["java", "lang", "String"]

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
    _                           -> JSyn.PrimType JSyn.LongT -- FIXME

giTypeToJNI :: GIType.Type -> CDSL.CTypeSpec
giTypeToJNI giType =
  case giType of
    (GIType.TBasicType t) -> CDSL.ty . fromString . giBasicTypeToJNI $ t
    -- FIXME: all the other ttypes
    _                     -> CDSL.ty . fromString . giBasicTypeToJNI $ GIType.TLong
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
