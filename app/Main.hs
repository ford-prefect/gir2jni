module Main where

import Data.GI.CodeGen.JNI

import qualified Data.Map as M (mapKeys)
import Data.Foldable (traverse_)
import qualified Data.Text as T (Text, lines, pack, unpack, toLower)
import qualified Data.Text.IO as TIO (readFile)
import Data.Tuple (swap)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory)

import Data.GI.CodeGen.API (girNSName, loadGIRInfo)
import Data.GI.CodeGen.Overrides (Overrides, filterAPIsAndDeps, girFixups, parseOverridesFile)

data Opts = Opts { optGIRName    :: T.Text
                 , optGIRVersion :: Maybe T.Text
                 , optOverrides  :: Maybe FilePath -- This will need to become a list
                 , optOutputDir  :: String
                 , optVerbose    :: Bool   }

parseOptions :: Parser Opts
parseOptions = Opts
           <$> (T.pack <$> strOption
               ( long "gir-name"
              <> short 'n'
              <> metavar "GIR"
              <> help "GIR package name to generate bindigs for" ) )
           <*> option (Just . T.pack <$> str)
               ( long "gir-version"
              <> short 'V'
              <> metavar "VER"
              <> value Nothing
              <> help "GIR package version to use" )
           <*> option (Just <$> str)
               ( long "gir-override"
              <> short 'o'
              <> metavar "OV"
              <> value Nothing
              <> help "GIR override file" )
           <*> strOption
               ( long "output"
              <> short 'O'
              <> value "."
              <> metavar "DIR"
              <> help "Output path for generated Java and C code" )
           <*> switch
               ( long "verbose"
              <> short 'v'
              <> help "Verbose output")


run :: Opts -> IO ()
run opts = do
  ovsFile   <- case optOverrides opts of
                 Nothing -> return []
                 Just f  -> T.lines <$> TIO.readFile f
  -- Parsing an empty file seems to work, but if not, we should use mempty
  ovsParsed <- parseOverridesFile ovsFile
  let ovs = case ovsParsed of
              Left err -> error ("Error parsing overrides file: " <> T.unpack err)
              Right o  -> o
  (gir, girDeps) <- loadGIRInfo (optVerbose opts) (optGIRName opts) (optGIRVersion opts) [] (girFixups ovs)
  let (apis, deps)    = filterAPIsAndDeps ovs gir girDeps
      (jFiles, cFile) = genJNI ["org", "freedesktop"] apis deps
      jPath           = optOutputDir opts </> "java"
      cPath           = optOutputDir opts </> "jni"
      cName           = (T.unpack . T.toLower . girNSName $ gir) <.> "c"
      outJFiles       = map (fmap (jPath </>)) jFiles
  createDirectoryIfMissing True cPath
  traverse_ (createDirectoryIfMissing True . takeDirectory . snd) outJFiles
  traverse_ (uncurry writeFile . swap) outJFiles
  writeFile (cPath </> cName) cFile

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> header "gir2jni - Java+JNI generation from GObject Introspection data" )
