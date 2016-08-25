{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GI.CodeGen.JNI.Function where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isNothing, maybeToList)
import Data.String (fromString)
import qualified Data.Text as T (Text, unpack)

import qualified Language.Java.Syntax as JSyn
import qualified Language.Java.Pretty as JPretty

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

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

genReturnCIdent :: String
genReturnCIdent = giCVarPrefix ++ "ret"

-- `empty` distinguishes between the declaration case (True) and the case
-- where we use genArgCDecl for creating a declaration for a castTo.
genArgCDecl :: Info -> Bool -> GI.Arg -> CDSL.CDecl
genArgCDecl info empty arg@GI.Arg{..} =
  let
    (typ, isPtr) = giTypeToC info argType
    ident        = if empty
                   then emptyDecl
                   else fromString . giArgToCIdent $ arg
  in
    makeTypeDecl isPtr ident typ

genReturnCDecl :: Info -> GIType.Type -> CDSL.CDecl
genReturnCDecl info giType =
  let
    (typ, isPtr) = giTypeToC info giType
    ident        = fromString genReturnCIdent
  in
    makeTypeDecl isPtr ident typ

genArgCInitAndCleanup :: Info -> GI.Arg -> (CDSL.CStat, Maybe CDSL.CStat)
genArgCInitAndCleanup info@Info{..} arg@GI.Arg{..} =
  let
    prefix = JSyn.Ident <$> infoPkgPrefix
    jniEnv = fromString jniEnvArg
    jniArg = fromString . giArgToJNIIdent $ arg
    cVar   = fromString . giArgToCIdent $ arg
    cType  = genArgCDecl info True arg
    -- FIXME: Do we need to deal with ownership transfer here?
    init   =
      if giTypeToJava prefix argType == javaStringType
        then
          cifElse (jniArg /=: 0)
            (hBlock [
              cVar <-- (star jniEnv &* "GetStringUTFChars")#[jniEnv, jniArg, 0]
              -- FIXME: Do an exception check and return if we have an exception
            ])
            (hBlock [
              cVar <-- 0
            ])
        else
          liftE $
            cVar <-- (jniArg `castTo` cType)
    -- FIXME: Do we need to deal with ownership transfer here?
    cleanup =
      if giTypeToJava prefix argType == javaStringType
        then
          Just $ cif (jniArg /=: 0)
            (hBlock [
              (star jniEnv &* "GetStringUTFChars")#[jniEnv, jniArg, cVar]
            ])
        else
          Nothing
  in
    (init, cleanup)

genFunctionCCall :: GI.Function -> CDSL.CStat
genFunctionCCall GI.Function{..} =
  let
    fn   = fromString . T.unpack $ fnSymbol
    args = fromString . giArgToCIdent <$> GI.args fnCallable
    ret  = fromString genReturnCIdent
  in
    if isNothing . GI.returnType $ fnCallable
    then
      liftE $ fn # args
    else
      liftE $ ret <-- fn # args

genFunctionCReturn :: Info -> Maybe GIType.Type -> CStat
genFunctionCReturn Info{..} giType =
  let
    ident   = fromString genReturnCIdent
    retType = giTypeToJNI giType
    retCast = makeTypeDecl False emptyDecl retType
  in
    if isNothing giType
    then
      cvoidReturn
    else
      creturn $ ident `castTo` retCast

genFunctionCDefn :: Info -> GI.Function -> [CDSL.CBlockItem]
genFunctionCDefn info@Info{..} func@GI.Function{..} =
  let
    retDecl = maybeToList $ genReturnCDecl info <$> GI.returnType fnCallable
    decls   = genArgCDecl info False <$> GI.args fnCallable
    ic      = genArgCInitAndCleanup info <$> GI.args fnCallable
    init    = fst <$> ic
    cleanup = catMaybes $ snd <$> ic
    call    = [genFunctionCCall func]
    ret     = [genFunctionCReturn info . GI.returnType $ fnCallable]
  in
    (CDSL.intoB <$> retDecl ++ decls) ++
    (CDSL.intoB <$> init ++ call ++ cleanup ++ ret)

genFunctionCDecl :: Info -> GI.Name -> GI.Function -> CDSL.CExtDecl
genFunctionCDecl info@Info{..} giName func@GI.Function{..} =
  let
    retType = CDSL.CTypeSpec . giTypeToJNI . GI.returnType $ fnCallable
    name    = giNameToJNI infoPkgPrefix giName
    cargs   = genFunctionCArgs . GI.args $ fnCallable
    defn    = genFunctionCDefn info func
  in
    export $ fun [retType] name cargs $ block defn

genFunctionDecl :: Info -> GI.Name -> GI.API -> Maybe (JSyn.Decl, CDSL.CExtDecl)
genFunctionDecl info@Info{..} giName (GI.APIFunction func) =
  if isValidFunction func
  then
    Just (genFunctionJavaDecl infoPkgPrefix giName (GI.fnCallable func),
          genFunctionCDecl info giName func)
  else
    Nothing
  where
    isValidFunction GI.Function{..} =
      -- FIXME: how do we deal with each of these cases?
      isNothing fnMovedTo &&                      -- function moved?
      (all (not . isOutArg) $ GI.args fnCallable) -- out argument(s)
    isOutArg GI.Arg{..} =
      direction /= GI.DirectionIn
genFunctionDecl _ _ _ = Nothing -- Ignore non-functions

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

genFunctions :: Info -> (M.Map FQClass JSyn.CompilationUnit, [CDSL.CExtDecl])
genFunctions info@Info{..} =
  let
    declsMaybe = M.mapWithKey (genFunctionDecl info) infoAPI   -- Map GI.Name   Maybe (JDecl, CDecl)
    declsList  = M.map maybeToList declsMaybe                  -- Map GI.Name   [(JDecl, CDecl)]
    decls      = M.mapKeysWith (++) makePackagePair declsList  -- Map (pkg, ns) [(JDecl, CDecl)]
    jdecls     = fmap fst <$> decls
    cdecls     = fmap snd <$> decls
    jcode      = M.mapWithKey (uncurry genFunctionJava) jdecls -- Map (pkg, ns) JCompilationUnit
    ccode      = concat cdecls
  in
    (jcode, ccode)
  where
    makePackagePair ns = (giNamespaceToJava infoPkgPrefix ns, T.unpack . GI.namespace $ ns)
