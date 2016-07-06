module Main where

import Lib

import qualified Data.Text as T (Text, pack)
import Options.Applicative

import Data.GI.CodeGen.API (loadGIRInfo, GIRInfo)

data Opts = Opts { optGIRName    :: T.Text
                 , optGIRVersion :: Maybe T.Text
                 , optOutputDir  :: String
                 , optVerbose    :: Bool   }

parseOptions :: Parser Opts
parseOptions = Opts
           <$> (T.pack <$> strOption
               ( long "gir-name"
              <> short 'n'
              <> metavar "GIR"
              <> help "GIR package name to generate bindigs for" ) )
           <*> option (str >>= return . Just . T.pack)
               ( long "gir-version"
              <> short 'V'
              <> metavar "VER"
              <> value Nothing
              <> help "GIR package version to use" )
           <*> strOption
               ( long "output"
              <> short 'o'
              <> value "-"
              <> metavar "DIR"
              <> help "Output path for generated Java and C code" )
           <*> switch
               ( long "verbose"
              <> short 'v'
              <> help "Verbose output")


run :: Opts -> IO ()
run opts = do
  (gir, girDeps) <- loadGIRInfo (optVerbose opts) (optGIRName opts) (optGIRVersion opts) [] []
  return ()

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> header "gir2jni - Java+JNI generation from GObject Introspection data" )
