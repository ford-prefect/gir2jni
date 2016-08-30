-- | Utility functions to simplify Java code generation

module Data.GI.CodeGen.JNI.Utils.Java where

import qualified Data.Text as T (toLower, unpack)
import qualified Data.Text.Manipulate as TManip (toCamel)

import qualified Data.GI.CodeGen.API as GI

import qualified Language.Java.Syntax as JSyn

import Data.GI.CodeGen.JNI.Utils.Type
import Data.GI.CodeGen.JNI.Types

giNamespaceToJava :: Package -> GI.Name -> Package
giNamespaceToJava pkg giName = pkg ++ [T.unpack . T.toLower . GI.namespace $ giName]

giClassNameToJava :: GI.Name -> String
giClassNameToJava = T.unpack . GI.name

giMethodNameToJava :: GI.Name -> String
giMethodNameToJava = T.unpack . TManip.toCamel . GI.name

giNameToJavaFQ :: Package -> GI.Name -> FQClass
giNameToJavaFQ pkg name = (giNamespaceToJava pkg name, giClassNameToJava name)

giArgToJava :: [JSyn.Ident] -> GI.Arg -> JSyn.FormalParam
giArgToJava prefix giArg =
  let
    var = JSyn.VarId . JSyn.Ident . T.unpack . GI.argCName $ giArg
    typ = giTypeToJava prefix . GI.argType $ giArg
  in
    JSyn.FormalParam [] typ False var

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
