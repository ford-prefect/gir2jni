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
import Data.GI.CodeGen.JNI.NativeObject
import Data.GI.CodeGen.JNI.Function
import Data.GI.CodeGen.JNI.Object

-- | Returns a list of Java files with their names, and the C code to be written
--   to a single file
genJNI :: Info -> ([(String, FilePath)], String)
genJNI info =
  let
    (j, c)     = foldl mergeCode (M.empty, []) [genNativeObject, genFunctions, genObjects]
    jPath      = M.mapKeys makePath j
    javaCode   = map swap . M.toList . M.map JPretty.prettyPrint $ jPath
    headerList = ["jni.h", "gst/gst.h"] -- FIXME: Discover headers from GIR
    headers    = concatMap (\h -> "#include <" ++ h ++ ">\n") headerList
    cCode      = headers ++
                 (TPretty.render . CDSL.pretty . CDSL.transUnit $ c)
  in
    (javaCode, cCode)
  where
    makePath (pkg, cls) = foldl1 (</>) (pkg ++ [cls <.> "java"])
    mergeCode (j, c) gen =
      let
        (j', c') = gen info
      in
        (M.union j j', c ++ c')
