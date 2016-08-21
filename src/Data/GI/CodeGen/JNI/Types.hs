module Data.GI.CodeGen.JNI.Types where

import qualified Data.Map as M
import qualified Data.Text as T

import qualified Data.GI.CodeGen.API as GI

-- | Lists component of the package (like ["org", "freedesktop"])
type Package = [String]

-- | Unqualified class name
type Class = String

-- | Fully qualified class consisting of the package and class name
type FQClass = (Package, Class)

-- | Information used during generation, based on Data.GI.CodeGen.API.GIRInfo
data Info = Info {
  infoPkgPrefix :: Package,              -- ^ Top level Java package prefix
  infoAPI       :: M.Map GI.Name GI.API, -- ^ The main API
  infoDeps      :: M.Map GI.Name GI.API, -- ^ The dependent APIs
  infoCTypes    :: M.Map GI.Name T.Text  -- ^ A map from GI types to the corresponding C type
}
