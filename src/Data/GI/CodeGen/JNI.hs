module Data.GI.CodeGen.JNI
    ( genJNI
    , Package
    ) where

import qualified Data.Map as M
import Data.Tuple (swap)
import System.FilePath ((</>), (<.>))

import Data.GI.CodeGen.API as GI

import Language.Java.Syntax as JSyn
import Language.Java.Pretty as JPretty

import Language.C.Pretty as CPretty
import Language.C.Syntax as CSyn

import Data.GI.CodeGen.JNI.Types
import Data.GI.CodeGen.JNI.Function (genFunctions)

-- | Returns a list of Java files with their names, and the C code to be written
--   to a single file
genJNI :: Package -> M.Map GI.Name GI.API -> M.Map GI.Name GI.API -> ([(String, FilePath)], String)
genJNI packagePrefix apis deps =
  let
    (jFun, cFun) = genFunctions packagePrefix (M.union apis deps)
    jPathFun     = M.mapKeys makePath jFun
    javaCode     = map swap . M.toList . M.map JPretty.prettyPrint $ jPathFun
    cCode        = concat $ show . CPretty.pretty <$> cFun -- FIXME (generate a CTranslationUnit)
  in
    (javaCode, cCode)
  where
    makePath (pkg, cls) = foldl1 (</>) (pkg ++ [cls <.> "java"])
