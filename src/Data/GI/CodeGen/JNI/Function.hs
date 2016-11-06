{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI.Function (genFunctions) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (isNothing, maybeToList)
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

genFunctionJavaDecl :: Info -> GI.Name -> GI.Callable -> JSyn.Decl
genFunctionJavaDecl info giName GI.Callable{..} =
  let
    mods    = [JSyn.Public, JSyn.Static]
    retType = giTypeToJava info <$> returnType
    ident   = JSyn.Ident . giMethodNameToJava $ giName
    params  = giArgToJavaParam info <$> args
  in
    JSyn.MemberDecl $ genJavaNativeMethodDecl mods retType ident params

genFunctionDecl :: Info -> GI.Name -> GI.API -> Maybe (JSyn.Decl, CDSL.CExtDecl)
genFunctionDecl info giName (GI.APIFunction func@GI.Function{..}) =
  if isValidFunction func
  then
    Just (genFunctionJavaDecl info giName (GI.fnCallable func),
          genJNIMethod info giName (GI.namespace giName) False False fnSymbol fnThrows fnCallable)
  else
    Nothing
  where
    isValidFunction GI.Function{..} =
      -- FIXME: how do we deal with each of these cases?
      isNothing fnMovedTo &&                         -- function moved?
      all (not . giArgIsOutArg) (GI.args fnCallable) -- out argument(s)
genFunctionDecl _ _ _ = Nothing -- Ignore non-functions

genFunctions :: Info -> (M.Map FQClass JSyn.CompilationUnit, [CDSL.CExtDecl])
genFunctions info@Info{..} =
  let
    declsMaybe = M.mapWithKey (genFunctionDecl info) infoAPI  -- Map GI.Name   Maybe (JDecl, CDecl)
    declsList  = M.map maybeToList declsMaybe                 -- Map GI.Name   [(JDecl, CDecl)]
    decls      = M.mapKeysWith (++) makePackagePair declsList -- Map (pkg, ns) [(JDecl, CDecl)]
    jdecls     = fmap fst <$> decls
    cdecls     = fmap snd <$> decls
    jcode      = M.mapWithKey genFunctionJava jdecls          -- Map (pkg, ns) JCompilationUnit
    ccode      = concat cdecls
  in
    (jcode, ccode)
  where
    makePackagePair ns = (giNamespaceToJava infoPkgPrefix ns, T.unpack . GI.namespace $ ns)
    genFunctionJava (pkg, ns) = genJavaClass pkg ns [JSyn.Public] Nothing []
