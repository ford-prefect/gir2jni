{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI.Function where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (isNothing, maybeToList)
import Data.String (fromString)
import qualified Data.Text as T (unpack)

import qualified Language.Java.Syntax as JSyn
import qualified Language.Java.Pretty as JPretty

import qualified Language.C.DSL as CDSL

import qualified Data.GI.CodeGen.API as GI

import Data.GI.CodeGen.JNI.Utils
import Data.GI.CodeGen.JNI.Types

genFunctionJavaDecl :: Package -> GI.Name -> GI.Callable -> JSyn.Decl
genFunctionJavaDecl packagePrefix giName GI.Callable{..} =
  let
    mods    = [JSyn.Public, JSyn.Static, JSyn.Native]
    prefix  = JSyn.Ident <$> packagePrefix
    retType = giTypeToJava prefix <$> returnType
    ident   = JSyn.Ident . giNameToJava $ giName
    params  = giArgToJava prefix <$> args
    body    = JSyn.MethodBody Nothing
  in
    JSyn.MemberDecl (JSyn.MethodDecl mods [] retType ident params [] body)

genFunctionCArgs :: [GI.Arg] -> [Maybe CDSL.CExpr -> CDSL.CDecl]
genFunctionCArgs args =
  jniEnvDecl : jniClassDecl : (giArgToJNI <$> args)

genFunctionCDecl :: Package -> GI.Name -> GI.Callable -> CDSL.CExtDecl
genFunctionCDecl packagePrefix giName GI.Callable{..} =
  let
    retType = CDSL.CTypeSpec . giTypeToJNI $ returnType
    name    = giNameToJNI packagePrefix giName
    cargs   = genFunctionCArgs args
  in
    CDSL.export $ CDSL.fun [retType] name cargs $ CDSL.block []

genFunctionDecl :: Package -> GI.Name -> GI.API -> Maybe (JSyn.Decl, CDSL.CExtDecl)
genFunctionDecl packagePrefix giName (GI.APIFunction func) =
  if isNothing . GI.fnMovedTo $ func
  then
    Just (genFunctionJavaDecl packagePrefix giName (GI.fnCallable func),
          genFunctionCDecl    packagePrefix giName (GI.fnCallable func))
  else
    Nothing -- FIXME: Generate a Java class for these
genFunctionDecl packagePrefix giName _                     = Nothing -- Ignore non-functions

-- | Generate the Java code for the given package, namespace, methods
genFunctionJava :: Package -> String -> [JSyn.Decl] -> JSyn.CompilationUnit
genFunctionJava packageStr nsStr methods =
  let
    package = Just . JSyn.PackageDecl . JSyn.Name $ (JSyn.Ident <$> packageStr)
    ns      = JSyn.Ident nsStr
    cls     = JSyn.ClassTypeDecl $ JSyn.ClassDecl [JSyn.Public] ns [] Nothing [] body
    body    = JSyn.ClassBody methods
  in
    JSyn.CompilationUnit package [] [cls]

genFunctions :: Package -> M.Map GI.Name GI.API -> (M.Map FQClass JSyn.CompilationUnit, [CDSL.CExtDecl])
genFunctions packagePrefix apis =
  let
    declsMaybe = M.mapWithKey (genFunctionDecl packagePrefix) apis -- Map GI.Name   Maybe (JDecl, CDecl)
    declsList  = M.map maybeToList declsMaybe                      -- Map GI.Name   [(JDecl, CDecl)]
    decls      = M.mapKeysWith (++) makePackagePair declsList      -- Map (pkg, ns) [(JDecl, CDecl)]
    jdecls     = fmap fst <$> decls
    cdecls     = fmap snd <$> decls
    jcode      = M.mapWithKey (uncurry genFunctionJava) jdecls     -- Map (pkg, ns) JCompilationUnit
    ccode      = concat cdecls
  in
    (jcode, ccode)
  where
    makePackagePair ns = (giNamespaceToJava packagePrefix ns, T.unpack . GI.namespace $ ns)
