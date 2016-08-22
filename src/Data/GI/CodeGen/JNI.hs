module Data.GI.CodeGen.JNI
    ( genJNI
    , Package
    , Info(..)
    ) where

import qualified Data.Map as M
import Data.Tuple (swap)
import System.FilePath ((</>), (<.>))
import qualified Text.PrettyPrint as TPretty

import qualified Data.GI.CodeGen.API as GI

import qualified Language.Java.Syntax as JSyn
import qualified Language.Java.Pretty as JPretty

import qualified Language.C.DSL as CDSL

import Data.GI.CodeGen.JNI.Types
import Data.GI.CodeGen.JNI.Function (genFunctions)

-- | Returns a list of Java files with their names, and the C code to be written
--   to a single file
genJNI :: Info -> ([(String, FilePath)], String)
genJNI info =
  let
    (jFun, cFun) = genFunctions info
    jPathFun     = M.mapKeys makePath jFun
    javaCode     = map swap . M.toList . M.map JPretty.prettyPrint $ jPathFun
    -- FIXME: Need headers
    cCode        = TPretty.render . CDSL.pretty . CDSL.transUnit $ cFun
  in
    (javaCode, cCode)
  where
    makePath (pkg, cls) = foldl1 (</>) (pkg ++ [cls <.> "java"])
