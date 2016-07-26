module Data.GI.CodeGen.JNI.Utils where

import Data.Char as C (toLower)
import Data.Text as T (toLower, unpack)
import System.FilePath ((</>), (<.>))

import Data.GI.CodeGen.API as GI
import Data.GI.CodeGen.Type as GIType

import Language.Java.Syntax as JSyn
import Language.Java.Pretty as JPretty

import Data.GI.CodeGen.JNI.Types

makeClassRef :: [JSyn.Ident] -> JSyn.Type
makeClassRef = JSyn.RefType . JSyn.ClassRefType . JSyn.ClassType . fmap (\id -> (id, []))

javaStringType :: JSyn.Type
javaStringType = makeClassRef . fmap JSyn.Ident $ ["java", "lang", "String"]

-- Java doesn't have unsigned types, so we promote all unsigned types to
-- the next wider Java signed type (except long, where we just have to hope
-- there aren't overflows -- writing bindings is fun.)
giTypeToJava :: [JSyn.Ident] -> GIType.Type -> JSyn.Type
giTypeToJava prefix giType =
  case giType of
    (GIType.TBasicType t)       -> giBasicTypeToJava t
    (GIType.TInterface cls ref) -> makeClassRef (prefix `mappend` (JSyn.Ident . T.unpack <$> [T.toLower cls, ref]))
    _                           -> JSyn.PrimType JSyn.LongT -- FIXME
  where
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

giArgToJava :: [JSyn.Ident] -> GI.Arg -> JSyn.FormalParam
giArgToJava prefix giArg =
  let
    var = JSyn.VarId . JSyn.Ident . T.unpack . GI.argCName $ giArg
    typ = giTypeToJava prefix . GI.argType $ giArg
  in
    JSyn.FormalParam [] typ False var

nameToFilePath :: Package -> String -> String -> FilePath
nameToFilePath package namespace cls =
  foldl1 (</>) (package ++ [C.toLower <$> namespace, cls <.> "java"])
