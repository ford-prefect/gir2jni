{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI
    ( genJNI
    , Package
    ) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text as T (unpack)
import Data.Tuple (swap)

import Data.GI.CodeGen.API as GI
import Data.GI.CodeGen.Type as GIType

import Language.Java.Syntax as JSyn
import Language.Java.Pretty as JPretty

import Data.GI.CodeGen.JNI.Types
import Data.GI.CodeGen.JNI.Utils

genFunctionDecl :: Package -> GI.Name -> GI.API -> [JSyn.Decl]
genFunctionDecl packagePrefix giName (GI.APIFunction func) =
  if isJust . GI.fnMovedTo $ func
  then []
  else
    let
      GI.Callable{..} = GI.fnCallable func
      prefix          = JSyn.Ident <$> packagePrefix
      retType         = giTypeToJava prefix <$> returnType
      id              = JSyn.Ident . T.unpack . GI.name $ giName
      params          = giArgToJava prefix <$> args
      body            = MethodBody Nothing
    in
      [JSyn.MemberDecl (JSyn.MethodDecl [JSyn.Public, JSyn.Static, JSyn.Native] [] retType id params [] body)]
-- Ignore non-functions
genFunctionDecl packagePrefix giName _ = []

-- | Generate the Java code for the given package, namespace, methods
genFunctionCU :: Package -> String -> [JSyn.Decl] -> JSyn.CompilationUnit
genFunctionCU packageStr nsStr methods =
  let
    package = Just . JSyn.PackageDecl . JSyn.Name $ (JSyn.Ident <$> (packageStr ++ [toLower <$> nsStr]))
    ns      = JSyn.Ident nsStr
    cls     = JSyn.ClassTypeDecl $ JSyn.ClassDecl [JSyn.Public] ns [] Nothing [] body
    body    = JSyn.ClassBody methods
  in
    JSyn.CompilationUnit package [] [cls]

genFunctions :: Package -> M.Map GI.Name GI.API -> M.Map FilePath JSyn.CompilationUnit
genFunctions packagePrefix apis =
  let
    decls       = M.mapWithKey (genFunctionDecl packagePrefix) apis  -- Map GI.Name [single Decl]
    functions   = M.mapKeysWith (++) (T.unpack . GI.namespace) decls -- Map namespace [decls]
    makePath ns = nameToFilePath packagePrefix ns ns
  in
    M.mapKeys makePath . M.mapWithKey (genFunctionCU packagePrefix) $ functions

-- | Returns a list of Java files with their names, and the C code to be written
--   to a single file
genJNI :: Package -> M.Map GI.Name GI.API -> M.Map GI.Name GI.API -> ([(String, FilePath)], String)
genJNI packagePrefix apis deps =
  let
    functions = genFunctions packagePrefix (M.union apis deps)
    javaCode  = map swap . M.toList . M.map JPretty.prettyPrint $ functions
    cCode     = ""
  in
    (javaCode, cCode)
