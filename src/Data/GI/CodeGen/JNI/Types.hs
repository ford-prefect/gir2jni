module Data.GI.CodeGen.JNI.Types where

import Data.GI.CodeGen.API as GI
import Language.Java.Syntax as JSyn
import Language.C.Syntax as CSyn

-- | Lists component of the package (like ["org", "freedesktop"])
type Package = [String]

-- | Unqualified class name
type Class = String

-- | Fully qualified class consisting of the package and class name
type FQClass = (Package, Class)
