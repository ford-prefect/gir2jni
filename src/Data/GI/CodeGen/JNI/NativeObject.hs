{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI.NativeObject
  ( genNativeObject
  , nativeObjectIdent
  , nativeObjectSetterIdent
  ) where

import qualified Language.Java.Syntax as JSyn

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import Data.GI.CodeGen.JNI.Utils.Java
import Data.GI.CodeGen.JNI.Types

nativeObjectIdent :: Class
nativeObjectIdent = "NativeObject"

nativeObjectFieldIdent :: String
nativeObjectFieldIdent = "pointer"

nativeObjectSetterIdent :: String
nativeObjectSetterIdent = "_setPointer"

genNativeObjectJava :: Info -> JSyn.CompilationUnit
genNativeObjectJava Info{..} =
  let
    pkg       = infoPkgPrefix
    name      = nativeObjectIdent
    modifiers = [JSyn.Public, JSyn.Abstract]
    ptrIdent  = JSyn.VarId . JSyn.Ident $ nativeObjectFieldIdent
    ptrInit   = Just. JSyn.InitExp . JSyn.Lit . JSyn.Int $ 0
    field     = JSyn.FieldDecl [] (JSyn.PrimType JSyn.LongT) [JSyn.VarDecl ptrIdent ptrInit]
    decls     = JSyn.MemberDecl <$> [field, nativeObjectSetterMethod]
  in
    genJavaClass pkg name modifiers Nothing [] decls
  where
    nativeObjectSetterMethod =
      let
        mods   = [JSyn.Protected, JSyn.Final]
        ident  = JSyn.Ident "object"
        param  = JSyn.FormalParam [] (JSyn.PrimType JSyn.LongT) False (JSyn.VarId ident)
        from   = JSyn.ExpName . JSyn.Name $ [ident]
        to     = JSyn.FieldLhs . JSyn.PrimaryFieldAccess JSyn.This . JSyn.Ident $ nativeObjectFieldIdent
        assign = JSyn.Assign to JSyn.EqualA from
        body   = JSyn.MethodBody . Just . JSyn.Block $ [JSyn.BlockStmt . JSyn.ExpStmt $ assign]
      in
        JSyn.MethodDecl mods [] Nothing (JSyn.Ident nativeObjectSetterIdent) [param] [] body

-- | NativeObject is a fake parent class of GObject that keeps a strong ref to
--   the object within the Java object, and drops the ref when the Java object
--   goes out of scope
genNativeObject :: Info -> ((FQClass, JSyn.CompilationUnit), [CDSL.CExtDecl])
genNativeObject info@Info{..} =
  let
    cls   = (infoPkgPrefix, nativeObjectIdent)
    jCode = genNativeObjectJava info
  in
    ((cls, jCode), [])
