module Data.GI.CodeGen.JNI
    ( genJNI
    , Package
    ) where

import qualified Data.Map as M
import Data.GI.CodeGen.API

-- Package name with each component being a list element (e.g. ["org", "freedesktop"]
type Package = [String]

-- Returns a list of Java files with their names, and the C code to be written
-- to a single file
genJNI :: Package -> M.Map Name API -> M.Map Name API -> ([(String, FilePath)], String)
genJNI packagePrefix apis deps = ([], "")
