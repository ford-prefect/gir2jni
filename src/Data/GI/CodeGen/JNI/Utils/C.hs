-- | Utility functions for C code generation

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GI.CodeGen.JNI.Utils.C where

import Data.List (intercalate)
import Data.Maybe (catMaybes, isNothing, maybeToList)
import Data.String (fromString)
import qualified Data.Text as T (Text, unpack)

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

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

jniInstanceDecl :: Maybe CDSL.CExpr -> CDSL.CDecl
jniInstanceDecl = jniTypeDefDecl "jobject" "thiz" False

giCVarPrefix :: String
giCVarPrefix = "c_"

giArgToCIdent :: GI.Arg -> String
giArgToCIdent GI.Arg{..} = giCVarPrefix ++ T.unpack argCName

giNameToJNI :: Package -> GI.Name -> T.Text -> String
giNameToJNI packagePrefix giName cls =
  intercalate "_" $ ["Java"]
                  ++ giNamespaceToJava packagePrefix giName
                  ++ [T.unpack cls, giMethodNameToJava giName]

giArgToJNIIdent :: GI.Arg -> String
giArgToJNIIdent GI.Arg{..} = T.unpack argCName

giArgToJNI :: GI.Arg -> (Maybe CDSL.CExpr -> CDSL.CDecl)
giArgToJNI arg@GI.Arg{..} =
  let
    typ  = CDSL.CTypeSpec . giTypeToJNI $ argType
    name = fromString . giArgToJNIIdent $ arg
  in
    CDSL.decl typ name

genJNIMethod :: Info
             -> GI.Name       -- ^ API method name
             -> T.Text        -- ^ Class name
             -> Bool          -- ^ Is instance method? (else class method)
             -> Bool          -- ^ Is constructor?
             -> T.Text        -- ^ Native method name
             -> Bool          -- ^ Throws error?
             -> GI.Callable   -- ^ Corresponding GI.Callable
             -> CDSL.CExtDecl -- ^ Return C declaration + definition
genJNIMethod info@Info{..} giName cls isInstance isConstr symbol throws callable =
  let
    retType  = GI.returnType callable
    retCType = case retType of
                 Nothing  -> CDSL.voidTy
                 Just typ -> CDSL.CTypeSpec . giTypeToJNI $ typ
    name    = giNameToJNI infoPkgPrefix giName cls
    cargs   = genFunctionCArgs isInstance . GI.args $ callable
    defn    = genFunctionCDefn info isConstr symbol throws callable
  in
    export $ fun [retCType] name cargs $ block defn
  where
    genFunctionCArgs :: Bool -> [GI.Arg] -> [Maybe CDSL.CExpr -> CDSL.CDecl]
    genFunctionCArgs isInstance args =
      let
        second = if isInstance
                 then jniInstanceDecl
                 else jniClassDecl
      in
        jniEnvDecl : second : (giArgToJNI <$> args)

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

    genErrorCDecl :: Info -> Bool -> Maybe CDSL.CDecl
    genErrorCDecl info throws =
      let
        ident        = fromString genErrorCIdent
        (typ, isPtr) = giTypeToC info GIType.TError
      in
        if throws
        then
          Just $ makeTypeDecl isPtr ident typ
        else
          Nothing

    genArgCInitAndCleanup :: Info -> GI.Arg -> (CDSL.CStat, Maybe CDSL.CStat)
    genArgCInitAndCleanup info@Info{..} arg@GI.Arg{..} =
      let
        jniEnv = fromString jniEnvArg
        jniArg = fromString . giArgToJNIIdent $ arg
        cVar   = fromString . giArgToCIdent $ arg
        cType  = genArgCDecl info True arg
        -- FIXME: We need to deal with ownership transfer here
        init   =
          if giTypeToJava info argType == javaStringType
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
          if giTypeToJava info argType == javaStringType
            then
              Just $ cif (jniArg /=: 0)
                (hBlock [
                  (star jniEnv &* "ReleaseStringUTFChars")#[jniEnv, jniArg, cVar]
                ])
            else
              Nothing
      in
        (init, cleanup)

    genErrorCInit :: Bool -> Maybe CDSL.CStat
    genErrorCInit throws =
      let
        err = fromString genErrorCIdent
      in
        if throws
        then
          Just . liftE $ err <-- 0
        else
          Nothing

    genFunctionCCall :: Bool -> T.Text -> Bool -> GI.Callable -> [CDSL.CStat]
    genFunctionCCall isConstr symbol throws callable =
      let
        fn        = fromString . T.unpack $ symbol
        err       = fromString genErrorCIdent
        errArg    = if throws
                    then
                      Just $ Addr `pre` err
                    else
                      Nothing
        args      = (fromString . giArgToCIdent <$> GI.args callable) ++ maybeToList errArg
        ret       = fromString genReturnCIdent
        call      = fn # args
        callExp   = if isNothing . GI.returnType $ callable
                      then
                        liftE call
                      else
                        if isConstr && GI.returnTransfer callable == GI.TransferNothing
                        then
                          -- This is a floating ref, sink it
                          liftE $ ret <-- "g_object_ref_sink" # [call]
                        else
                          liftE $ ret <-- call
        -- FIXME: log the error
        handleErr = [ cif err $ hBlock [ "g_error_free" # [err] ] | throws ]
      in
        callExp : handleErr

    genFunctionCReturn :: Info -> GI.Callable -> [CStat]
    genFunctionCReturn info@Info{..} GI.Callable{..} =
      let
        cIdent = fromString genReturnCIdent
        jIdent = fromString genReturnJNIIdent
      in
        case returnType of
          Nothing -> [cvoidReturn]
          Just t  -> genFunctionCToJNI info t cIdent jIdent :
                     [creturn jIdent]
      where
        retCast t =
          makeTypeDecl False emptyCDecl (giTypeToJNI t)

        genFunctionCToJNI info typ cVar jVar =
          let
            jniEnv = fromString jniEnvArg
          in
            if giTypeToJava info typ == javaStringType
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

    genFunctionCDefn :: Info -> Bool -> T.Text -> Bool -> GI.Callable -> [CDSL.CBlockItem]
    genFunctionCDefn info@Info{..} isConstr symbol throws callable =
      let
        retDecl = genReturnCDecl info . GI.returnType $ callable
        decls   = genArgCDecl info False <$> GI.args callable
        errDecl = maybeToList $ genErrorCDecl info throws
        ic      = genArgCInitAndCleanup info <$> GI.args callable
        errInit = genErrorCInit throws
        init    = (fst <$> ic) ++ maybeToList errInit
        cleanup = catMaybes $ snd <$> ic
        call    = genFunctionCCall isConstr symbol throws callable
        ret     = genFunctionCReturn info callable
      in
        (CDSL.intoB <$> retDecl ++ decls ++ errDecl) ++
        (CDSL.intoB <$> init ++ call ++ cleanup ++ ret)
