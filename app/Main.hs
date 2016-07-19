module Main where

import Lib

import qualified Data.Text as T (Text, lines, pack, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Options.Applicative

import Data.GI.CodeGen.API (loadGIRInfo)
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
              Left err -> error ("Error parsing overrides file: " `mappend` T.unpack err)
              Right o  -> o
  (gir, girDeps) <- loadGIRInfo (optVerbose opts) (optGIRName opts) (optGIRVersion opts) [] (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
  return ()

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> header "gir2jni - Java+JNI generation from GObject Introspection data" )
