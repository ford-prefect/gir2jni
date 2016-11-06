{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GI.CodeGen.JNI.Object (genObjects) where

import qualified Data.Map as M
import Data.Maybe (isJust, maybeToList)
import Data.String (fromString)
import qualified Data.Text as T (pack, unpack)

import qualified Language.Java.Syntax as JSyn

-- The idea is to use this qualified everywhere except when using as a DSL
import Language.C.DSL as CDSL

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

import Data.GI.CodeGen.JNI.NativeObject
import Data.GI.CodeGen.JNI.Utils.C
import Data.GI.CodeGen.JNI.Utils.Java
import Data.GI.CodeGen.JNI.Utils.Type
import Data.GI.CodeGen.JNI.Types

genRuntimeException :: String -> JSyn.Exp
genRuntimeException msg =
  let
    cls = javaClassType ["java", "lang", "RuntimeException"]
    arg = JSyn.Lit . JSyn.String $ msg
  in
    JSyn.InstanceCreation [] cls [arg] Nothing

genEmptyConstructor :: Info -> GI.Name -> JSyn.Decl
genEmptyConstructor Info{..} name =
      let
        ident  = JSyn.Ident . giClassNameToJava $ name
        body   = JSyn.ConstructorBody Nothing []
        cons   = JSyn.ConstructorDecl [JSyn.Public] [] ident [] [] body
      in
        JSyn.MemberDecl cons

genObjectConstructor :: Info -> GI.Name -> Int -> GI.Method -> ([JSyn.Decl], [CDSL.CExtDecl])
genObjectConstructor info name num method =
  let
    jDecls = genObjectConstructorJava info name method
    cDecls = genObjectConstructorC info name method
  in
    (jDecls, cDecls)
  where
    genObjectConstructorJava info name GI.Method{..} =
      let
        nativeConsIdent = JSyn.Ident consName
        nativeConsDecl  = genJavaNativeMethodDecl [] (Just . JSyn.PrimType $ JSyn.LongT) nativeConsIdent params

        ident  = JSyn.Ident . giClassNameToJava $ name
        params = giArgToJavaParam info <$> GI.args methodCallable

        -- ^ long object;
        var    = JSyn.Ident "object"
        vars   = JSyn.LocalVars [] (JSyn.PrimType JSyn.LongT) [JSyn.VarDecl (JSyn.VarId var) Nothing]

        -- object = nativeConstructor(...)
        to     = JSyn.NameLhs . JSyn.Name $ [var]
        args   = JSyn.ExpName . JSyn.Name . flip (:) [] . giArgToJavaName <$> GI.args methodCallable
        from   = JSyn.MethodInv . JSyn.MethodCall (JSyn.Name [nativeConsIdent]) $ args
        assign = JSyn.BlockStmt . JSyn.ExpStmt $ JSyn.Assign to JSyn.EqualA from

        -- _setObject(object)
        set    = JSyn.BlockStmt
               . JSyn.ExpStmt
               . JSyn.MethodInv
               . JSyn.SuperMethodCall [] (JSyn.Ident nativeObjectSetterIdent) $ [JSyn.ExpName . JSyn.Name $ [var]]

        body   = JSyn.ConstructorBody Nothing [vars, assign, set]
      in
        JSyn.MemberDecl <$> [nativeConsDecl, JSyn.ConstructorDecl [JSyn.Public] [] ident params [] body]

    genObjectConstructorC Info{..} name GI.Method{..} =
      let
        retType  = GIType.TBasicType GIType.TLong
        giName   = name { GI.name = fromString consName }
        cls      = GI.name name
      in
        [genJNIMethod info giName cls True True methodSymbol methodThrows methodCallable]

    consName = "nativeConstructor" ++ show num

genObject :: Info -> GI.Name -> GI.API -> Maybe ((FQClass, JSyn.CompilationUnit), [CDSL.CExtDecl])
genObject info@Info{..} name (GI.APIObject obj@GI.Object{..}) =
  let
    pkg    = giNamespaceToJava infoPkgPrefix name
    cls    = giClassNameToJava name
    fqCls  = (pkg, cls)
    parent = if giIsGObject name
             then
               Just (infoPkgPrefix, nativeObjectIdent)
             else
               giNameToJavaFQ infoPkgPrefix <$> objParent

    conss  = zip [1..] $ filter ((==) GI.Constructor . GI.methodType) objMethods -- "Indexed" list of constructors
    conss' = fmap (uncurry $ genObjectConstructor info name) conss
    cons   = foldl concatTuple ([], []) conss'
    empty  = maybeToList $
              if null conss || (any isEmptyConstructor . fmap snd $ conss)
              then
                Nothing
              else
                -- We must have an empty constructor for subclass constructors to work
                Just $ genEmptyConstructor info name

    -- FIXME: Enable after implementing interfaces
    -- ifaces = giNameToJavaFQ infoPkgPrefix <$> objInterfaces
    ifaces = []
    decls  = empty ++ fst cons
    jCode  = genJavaClass pkg cls [JSyn.Public] parent ifaces decls
    cCode  = snd cons
  in
    if giIsObjectType info . giNameToType $ name
    then
      Just ((fqCls, jCode), cCode)
    else
      Nothing -- Not a GObject-derived type, ignore
  where
    concatTuple (js, cs) (j, c) = (js ++ j, cs ++ c)
    isEmptyConstructor GI.Method{..} = methodType == GI.Constructor && (null . GI.args $ methodCallable)

genObject _ _ _ = Nothing

genObjects :: Info -> (M.Map FQClass JSyn.CompilationUnit, [CDSL.CExtDecl])
genObjects info@Info{..} =
  let
    apis      = M.union infoAPI infoDeps
    nativeObj = genNativeObject info
    -- Map GI.Name ((FQClass, JSyn.CompilationUnit), [CDSL.CExtDecl])
    objs      = M.mapMaybeWithKey (genObject info) apis
    objs'     = M.insert (GI.Name "" (T.pack nativeObjectIdent)) nativeObj objs
    jCode     = M.fromList . M.elems . fmap fst $ objs'
    cCode     = concatMap snd objs'
  in
    (jCode, cCode)
