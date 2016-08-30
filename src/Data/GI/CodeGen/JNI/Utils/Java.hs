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

giNameToJava :: GI.Name -> String
giNameToJava = T.unpack . TManip.toCamel . GI.name

giArgToJava :: [JSyn.Ident] -> GI.Arg -> JSyn.FormalParam
giArgToJava prefix giArg =
  let
    var = JSyn.VarId . JSyn.Ident . T.unpack . GI.argCName $ giArg
    typ = giTypeToJava prefix . GI.argType $ giArg
  in
    JSyn.FormalParam [] typ False var

-- | Generate the Java code for the given package, namespace, fields/methods
genJavaClass :: Package              -- ^ Package
             -> String               -- ^ Class name
             -> [JSyn.Modifier]      -- ^ Modifiers
             -> Maybe String         -- ^ Parent class name
             -> [String]             -- ^ Interfaces implemented
             -> [JSyn.Decl]          -- ^ Field, method declarations
             -> JSyn.CompilationUnit -- ^ Output code
genJavaClass packageStr name mods parent ifaces decls =
  let
    package = Just . JSyn.PackageDecl . JSyn.Name $ (JSyn.Ident <$> packageStr)
    ident   = JSyn.Ident name
    inherit = nameToRefType <$> parent
    impls   = nameToRefType <$> ifaces
    cls     = JSyn.ClassTypeDecl $ JSyn.ClassDecl mods ident [] inherit impls body
    body    = JSyn.ClassBody decls
  in
    JSyn.CompilationUnit package [] [cls]
  where
    nameToRefType n = JSyn.ClassRefType . JSyn.ClassType $ [(JSyn.Ident n, [])]
