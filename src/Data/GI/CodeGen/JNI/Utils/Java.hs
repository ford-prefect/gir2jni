-- | Utility functions to simplify Java code generation
{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI.Utils.Java where

import qualified Data.Text as T (unpack)
import qualified Data.Text.Manipulate as TManip (toCamel)

import qualified Data.GI.CodeGen.API as GI
import qualified Data.GI.CodeGen.Type as GIType

import qualified Language.Java.Syntax as JSyn

import Data.GI.CodeGen.JNI.Utils.Type
import Data.GI.CodeGen.JNI.Types

javaOverrideAnnotation :: JSyn.Modifier
javaOverrideAnnotation = JSyn.Annotation . JSyn.MarkerAnnotation . JSyn.Name $ [JSyn.Ident "Override"]

giClassNameToJava :: GI.Name -> String
giClassNameToJava = T.unpack . GI.name

giMethodNameToJava :: GI.Name -> String
giMethodNameToJava = T.unpack . TManip.toCamel . GI.name

giNameToJavaFQ :: Package -> GI.Name -> FQClass
giNameToJavaFQ pkg name = (giNamespaceToJava pkg name, giClassNameToJava name)

giTypeToJava :: Info -> GIType.Type -> JSyn.Type
giTypeToJava info@Info{..} giType =
  case giType of
    (GIType.TBasicType t)       -> giBasicTypeToJava t
    (GIType.TInterface cls ref) -> if giIsObjectType info giType
                                   then
                                     JSyn.RefType . javaFQToClassRef . giNameToJavaFQ infoPkgPrefix $ GI.Name cls ref
                                   else
                                     JSyn.PrimType JSyn.LongT -- FIXME
    _                           -> JSyn.PrimType JSyn.LongT -- FIXME

giArgToJavaName :: GI.Arg -> JSyn.Ident
giArgToJavaName GI.Arg{..} = JSyn.Ident . T.unpack $ argCName

giArgToJavaParam :: Info -> GI.Arg -> JSyn.FormalParam
giArgToJavaParam info giArg =
  let
    var = JSyn.VarId . giArgToJavaName $ giArg
    typ = giTypeToJava info . GI.argType $ giArg
  in
    JSyn.FormalParam [] typ False var

-- | Generate a native method member declaration
genJavaNativeMethodDecl :: [JSyn.Modifier]    -- ^ Modifiers (other than native)
                        -> Maybe JSyn.Type    -- ^ Return type
                        -> JSyn.Ident         -- ^ Method name
                        -> [JSyn.FormalParam] -- ^ Arguments
                        -> JSyn.MemberDecl
genJavaNativeMethodDecl mods retType ident params =
  let
    mods' = JSyn.Native : mods
    body  = JSyn.MethodBody Nothing
  in
    JSyn.MethodDecl mods' [] retType ident params [] body

-- | Generate the Java code for the given package, namespace, fields/methods
genJavaClass :: Package              -- ^ Package
             -> Class                -- ^ Class name
             -> [JSyn.Modifier]      -- ^ Modifiers
             -> Maybe FQClass        -- ^ Parent class name
             -> [FQClass]            -- ^ Interfaces implemented
             -> [JSyn.Decl]          -- ^ Field, method declarations
             -> JSyn.CompilationUnit -- ^ Output code
genJavaClass packageStr name mods parent ifaces decls =
  let
    package = Just . JSyn.PackageDecl . JSyn.Name $ (JSyn.Ident <$> packageStr)
    ident   = JSyn.Ident name
    inherit = javaFQToClassRef <$> parent
    impls   = javaFQToClassRef <$> ifaces
    cls     = JSyn.ClassTypeDecl $ JSyn.ClassDecl mods ident [] inherit impls body
    body    = JSyn.ClassBody decls
  in
    JSyn.CompilationUnit package [] [cls]
