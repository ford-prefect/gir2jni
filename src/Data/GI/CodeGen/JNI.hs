{-# LANGUAGE RecordWildCards #-}

module Data.GI.CodeGen.JNI
    ( genJNI
    , Package
    , Info(..)
    ) where

import qualified Data.Map as M
import Text.PrettyPrint as TPretty
import Data.Tuple (swap)
import System.FilePath ((</>), (<.>))

import Data.GI.CodeGen.API as GI

import Language.Java.Syntax as JSyn
import Language.Java.Pretty as JPretty

import Language.C.Data.Node as CNode (undefNode)
import Language.C.Syntax as CSyn
import Language.C.Pretty as CPretty

import Data.GI.CodeGen.JNI.Types
import Data.GI.CodeGen.JNI.Function (genFunctions)

-- | Returns a list of Java files with their names, and the C code to be written
--   to a single file
genJNI :: Info -> ([(String, FilePath)], String)
genJNI Info{..} =
  let
    (jFun, cFun) = genFunctions infoPkgPrefix (M.union infoAPI infoDeps)
    jPathFun     = M.mapKeys makePath jFun
    javaCode     = map swap . M.toList . M.map JPretty.prettyPrint $ jPathFun
    -- FIXME: Need headers
    cCode        = TPretty.render . CPretty.pretty . CSyn.CTranslUnit cFun $ CNode.undefNode
  in
    (javaCode, cCode)
  where
    makePath (pkg, cls) = foldl1 (</>) (pkg ++ [cls <.> "java"])
