-- | Utility functions for C code generation

{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI.Utils.C where

import Data.List (intercalate)
import Data.String (fromString)
import qualified Data.Text as T (unpack)

import qualified Data.GI.CodeGen.API as GI

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import Data.GI.CodeGen.JNI.Types
import Data.GI.CodeGen.JNI.Utils.Java
import Data.GI.CodeGen.JNI.Utils.Type

jniTypeDefDecl :: String -> String -> Bool -> Maybe CDSL.CExpr -> CDSL.CDecl
jniTypeDefDecl typ name isPtr =
  let
     typeSpec = CDSL.CTypeSpec . CDSL.ty . fromString $ typ
     doPtr    = if isPtr then CDSL.ptr else id
     ident    = doPtr . fromString $ name
  in
    CDSL.decl typeSpec ident

emptyCDecl :: CDSL.CDeclr
emptyCDecl = CDSL.CDeclr Nothing [] Nothing [] CDSL.undefNode

makeTypeDecl :: Bool -> CDSL.CDeclr -> CDSL.CTypeSpec -> CDSL.CDecl
makeTypeDecl isPtr ident typ =
  let
    maybePtr = if isPtr then ptr else id
  in
    decl (CDSL.CTypeSpec typ) (maybePtr ident) Nothing

typePtrDecl :: CDSL.CTypeSpec -> CDSL.CDecl
typePtrDecl = makeTypeDecl True emptyCDecl

jniNull :: CDSL.CExpr
jniNull =
    0 `castTo` typePtrDecl voidSpec

jniEnvArg :: String
jniEnvArg = "env"

jniEnvDecl :: Maybe CDSL.CExpr -> CDSL.CDecl
jniEnvDecl = jniTypeDefDecl "JNIEnv" jniEnvArg True

jniClassDecl :: Maybe CDSL.CExpr -> CDSL.CDecl
jniClassDecl = jniTypeDefDecl "jclass" "clazz" False

giCVarPrefix :: String
giCVarPrefix = "c_"

giArgToCIdent :: GI.Arg -> String
giArgToCIdent GI.Arg{..} = giCVarPrefix ++ T.unpack argCName

giNameToJNI :: Package -> GI.Name -> String
giNameToJNI packagePrefix giName =
  intercalate "_" $ ["Java"]
                  ++ giNamespaceToJava packagePrefix giName
                  ++ [T.unpack . GI.namespace $ giName, giMethodNameToJava giName]

giArgToJNIIdent :: GI.Arg -> String
giArgToJNIIdent GI.Arg{..} = T.unpack argCName

giArgToJNI :: GI.Arg -> (Maybe CDSL.CExpr -> CDSL.CDecl)
giArgToJNI arg@GI.Arg{..} =
  let
    typ  = CDSL.CTypeSpec . giTypeToJNI $ argType
    name = fromString . giArgToJNIIdent $ arg
  in
    CDSL.decl typ name
