{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GI.CodeGen.JNI.NativeObject
  ( genNativeObject
  , nativeObjectIdent
  , nativeObjectSetterIdent
  ) where

import Data.String (fromString)

import qualified Language.Java.Syntax as JSyn

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

import Data.GI.CodeGen.JNI.Utils.C
import Data.GI.CodeGen.JNI.Utils.Java
import Data.GI.CodeGen.JNI.Types

nativeObjectIdent :: Class
nativeObjectIdent = "NativeObject"

nativeObjectFieldIdent :: String
nativeObjectFieldIdent = "pointer"

nativeObjectSetterIdent :: String
nativeObjectSetterIdent = "_setPointer"

-- | NativeObject is a fake parent class of GObject that keeps a strong ref to
--   the object within the Java object, and drops the ref when the Java object
--   goes out of scope
genNativeObject :: Info -> ((FQClass, JSyn.CompilationUnit), [CDSL.CExtDecl])
genNativeObject info@Info{..} =
  let
    cls   = (infoPkgPrefix, nativeObjectIdent)
    jCode = genNativeObjectJava info
    cCode = genNativeObjectC info cls
  in
    ((cls, jCode), cCode)
  where
    nativeObjectDestrIdent = "nativeDestructor"

    genNativeObjectJava :: Info -> JSyn.CompilationUnit
    genNativeObjectJava Info{..} =
      let
        modifiers   = [JSyn.Public, JSyn.Abstract]

        ptrIdent    = JSyn.VarId . JSyn.Ident $ nativeObjectFieldIdent
        ptrInit     = Just. JSyn.InitExp . JSyn.Lit . JSyn.Int $ 0
        ptrField    = JSyn.FieldDecl [] (JSyn.PrimType JSyn.LongT) [JSyn.VarDecl ptrIdent ptrInit]

        param       = JSyn.FormalParam [] (JSyn.PrimType JSyn.LongT) False ptrIdent
        nativeDestr = genJavaNativeMethodDecl [] Nothing (JSyn.Ident nativeObjectDestrIdent) [param]

        decls       = JSyn.MemberDecl <$> [ptrField, setter, nativeDestr, finalize]
      in
        genJavaClass infoPkgPrefix nativeObjectIdent modifiers Nothing [] decls
      where
        thisPtr = JSyn.PrimaryFieldAccess JSyn.This . JSyn.Ident $ nativeObjectFieldIdent

        setter =
          let
            mods   = [JSyn.Protected, JSyn.Final]
            ident  = JSyn.Ident "object"
            param  = JSyn.FormalParam [] (JSyn.PrimType JSyn.LongT) False (JSyn.VarId ident)
            from   = JSyn.ExpName . JSyn.Name $ [ident]
            to     = JSyn.FieldLhs thisPtr
            assign = JSyn.Assign to JSyn.EqualA from
            body   = JSyn.MethodBody . Just . JSyn.Block $ [JSyn.BlockStmt . JSyn.ExpStmt $ assign]
          in
            JSyn.MethodDecl mods [] Nothing (JSyn.Ident nativeObjectSetterIdent) [param] [] body

        finalize =
          let
            fin   = JSyn.Ident "finalize"
            mods  = [JSyn.Protected, javaOverrideAnnotation]
            free  = JSyn.BlockStmt
                  . JSyn.ExpStmt
                  . JSyn.MethodInv
                  . JSyn.MethodCall (JSyn.Name $ [JSyn.Ident nativeObjectDestrIdent]) $
                                    [JSyn.FieldAccess thisPtr]
            -- Needed for suppressing a warning, but needs additional try-catch
            -- hoop jumping.
            --super = JSyn.BlockStmt
            --      . JSyn.ExpStmt
            --      . JSyn.MethodInv
            --      . JSyn.SuperMethodCall [] fin $ []
            body  = JSyn.MethodBody . Just . JSyn.Block $ [free]
          in
            JSyn.MethodDecl mods [] Nothing fin [] [] body

    genNativeObjectC :: Info -> FQClass -> [CDSL.CExtDecl]
    genNativeObjectC Info{..} fqClass =
      let
        giName     = GI.Name "" (fromString nativeObjectDestrIdent)

        -- jniGetObjectPointerIdent
        env        = fromString jniEnvIdent
        cls        = fromString jniClassIdent
        field      = fromString jniFieldIdent
        obj        = fromString jniInstanceIdent
        className  = str $ jniClassName fqClass

        destructor = giNameToJNI infoPkgPrefix giName (fromString nativeObjectIdent)
      in
        export <$> [
          -- FIXME: Pepper some exception checks in here
          fun [longTy] jniGetObjectPointerIdent [jniEnvDecl, jniInstanceDecl] $ functionBlock
            [
              uninit $ jniClassDecl,
              uninit $ jniFieldDecl,
              uninit $ long "ret"
            ] [
              cls   <-- (star env &* "FindClass")#[env, className],
              field <-- (star env &* "GetFieldID")#[env, cls, str nativeObjectFieldIdent, str "J"],
              "ret" <-- (star env &* "GetLongField")#[env, obj, field]
            ] [
              creturn "ret"
            ],

          fun [voidTy] destructor [long "ptr"] $ hBlock [
            "g_object_unref" # ["ptr"]
          ]
        ]
      where
        functionBlock decls stats ret = block $ (intoB <$> decls) ++ (intoB <$> stats) ++ (intoB <$> ret)
