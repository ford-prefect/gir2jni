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

import Data.GI.CodeGen.JNI.Utils.C
import Data.GI.CodeGen.JNI.Utils.Java
import Data.GI.CodeGen.JNI.Utils.Type
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
genReturnCIdent = giCVarPrefix ++ "_ret"

genReturnJNIIdent :: String
genReturnJNIIdent = "j_ret"

genErrorCIdent :: String
genErrorCIdent = giCVarPrefix ++ "err"

-- `empty` distinguishes between the declaration case (True) and the case
-- where we use genArgCDecl for creating a declaration for a castTo.
genArgCDecl :: Info -> Bool -> GI.Arg -> CDSL.CDecl
genArgCDecl info empty arg@GI.Arg{..} =
  let
    (typ, isPtr) = giTypeToC info argType
    ident        = if empty
                   then emptyCDecl
                   else fromString . giArgToCIdent $ arg
  in
    makeTypeDecl isPtr ident typ

genReturnCDecl :: Info -> Maybe GIType.Type -> [CDSL.CDecl]
genReturnCDecl info giType =
  case giType of
    Nothing -> []
    Just t  -> genReturnCDecl' info t
  where
    genReturnCDecl' info t =
      let
        (cType, isPtr) = giTypeToC info t
        jType          = giTypeToJNI t
        cIdent         = fromString genReturnCIdent
        jIdent         = fromString genReturnJNIIdent
      in
        [makeTypeDecl isPtr cIdent cType, makeTypeDecl False jIdent jType]

genErrorCDecl :: Info -> GI.Function -> Maybe CDSL.CDecl
genErrorCDecl info GI.Function{..} =
  let
    ident        = fromString genErrorCIdent
    (typ, isPtr) = giTypeToC info GIType.TError
  in
    if fnThrows
    then
      Just $ makeTypeDecl isPtr ident typ
    else
      Nothing

genArgCInitAndCleanup :: Info -> GI.Arg -> (CDSL.CStat, Maybe CDSL.CStat)
genArgCInitAndCleanup info@Info{..} arg@GI.Arg{..} =
  let
    prefix = JSyn.Ident <$> infoPkgPrefix
    jniEnv = fromString jniEnvArg
    jniArg = fromString . giArgToJNIIdent $ arg
    cVar   = fromString . giArgToCIdent $ arg
    cType  = genArgCDecl info True arg
    -- FIXME: We need to deal with ownership transfer here
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
              (star jniEnv &* "ReleaseStringUTFChars")#[jniEnv, jniArg, cVar]
            ])
        else
          Nothing
  in
    (init, cleanup)

genErrorCInit :: GI.Function -> Maybe CDSL.CStat
genErrorCInit GI.Function{..} =
  let
    err = fromString genErrorCIdent 
  in
    if fnThrows
    then
      Just . liftE $ err <-- 0
    else
      Nothing

genFunctionCCall :: GI.Function -> [CDSL.CStat]
genFunctionCCall GI.Function{..} =
  let
    fn        = fromString . T.unpack $ fnSymbol
    err       = fromString genErrorCIdent
    errArg    = if fnThrows
                then
                  Just $ Addr `pre` err
                else
                  Nothing
    args      = (fromString . giArgToCIdent <$> GI.args fnCallable) ++ maybeToList errArg
    ret       = fromString genReturnCIdent
    call      = if isNothing . GI.returnType $ fnCallable
                  then
                    liftE $ fn # args
                  else
                    liftE $ ret <-- fn # args
    -- FIXME: log the error
    handleErr = [ cif err $ hBlock [ "g_error_free" # [err] ] | fnThrows ]
  in
    call : handleErr

genFunctionCReturn :: Info -> GI.Callable -> [CStat]
genFunctionCReturn Info{..} GI.Callable{..} =
  let
    cIdent = fromString genReturnCIdent
    jIdent = fromString genReturnJNIIdent
  in
    case returnType of
      Nothing -> [cvoidReturn]
      Just t  -> genFunctionCToJNI t cIdent jIdent :
                 [creturn jIdent]
  where
    retCast t =
      makeTypeDecl False emptyCDecl (giTypeToJNI t)

    genFunctionCToJNI typ cVar jVar =
      let
        prefix = JSyn.Ident <$> infoPkgPrefix
        jniEnv = fromString jniEnvArg
      in
        if giTypeToJava prefix typ == javaStringType
        then
          cifElse cVar
            (hBlock $
              (jVar <-- (star jniEnv &* "NewStringUTF") # [jniEnv, cVar]) :
              [ "g_free" # [cVar] | returnTransfer /= GI.TransferEverything ]
            )
            (hBlock [
              jVar <-- 0
            ])
          else
            -- FIXME: Can't just do a simple assign every time
            liftE $ jVar <-- cVar `castTo` retCast typ

genFunctionCDefn :: Info -> GI.Function -> [CDSL.CBlockItem]
genFunctionCDefn info@Info{..} func@GI.Function{..} =
  let
    retDecl = genReturnCDecl info . GI.returnType $ fnCallable
    decls   = genArgCDecl info False <$> GI.args fnCallable
    errDecl = maybeToList $ genErrorCDecl info func
    ic      = genArgCInitAndCleanup info <$> GI.args fnCallable
    errInit = genErrorCInit func
    init    = (fst <$> ic) ++ maybeToList errInit
    cleanup = catMaybes $ snd <$> ic
    call    = genFunctionCCall func
    ret     = genFunctionCReturn info fnCallable
  in
    (CDSL.intoB <$> retDecl ++ decls ++ errDecl) ++
    (CDSL.intoB <$> init ++ call ++ cleanup ++ ret)

genFunctionCDecl :: Info -> GI.Name -> GI.Function -> CDSL.CExtDecl
genFunctionCDecl info@Info{..} giName func@GI.Function{..} =
  let
    retType  = GI.returnType fnCallable
    retCType = case retType of
                 Nothing  -> CDSL.voidTy
                 Just typ -> CDSL.CTypeSpec . giTypeToJNI $ typ
    name    = giNameToJNI infoPkgPrefix giName
    cargs   = genFunctionCArgs . GI.args $ fnCallable
    defn    = genFunctionCDefn info func
  in
    export $ fun [retCType] name cargs $ block defn

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
      isNothing fnMovedTo &&                    -- function moved?
      all (not . isOutArg) (GI.args fnCallable) -- out argument(s)

    isOutArg GI.Arg{..} =
      -- FIXME: we ignore the out part of inout arguments
      direction == GI.DirectionOut
genFunctionDecl _ _ _ = Nothing -- Ignore non-functions

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
