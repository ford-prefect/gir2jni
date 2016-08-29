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
giNameToJava =
  T.unpack . toCamelCase . GI.name
  where
    toCamelCase = TManip.toCamel

giArgToJava :: [JSyn.Ident] -> GI.Arg -> JSyn.FormalParam
giArgToJava prefix giArg =
  let
    var = JSyn.VarId . JSyn.Ident . T.unpack . GI.argCName $ giArg
    typ = giTypeToJava prefix . GI.argType $ giArg
  in
    JSyn.FormalParam [] typ False var

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
