module Bore.CLI (defaultEntryPoint) where


import Bore.FileLayout
import Bore.Library
import Bore.Parse (buildTree)
import Bore.Config
import qualified Bore.ToJekyll as ToJekyll (buildTree)

import Data.Text (pack)
import System.Directory (canonicalizePath)
import Options.Applicative

{- | Determine the source and output directories.

Takes two Maybe FilePaths and returns the source and output directories based on defaults
and canonicalizes them.
-}
determineDirectories :: Maybe FilePath -> Maybe FilePath -> IO (AbsolutePath, AbsolutePath)
determineDirectories maybeSourcePath maybeOutputPath = do
  sourcePath <- case maybeSourcePath of
    Just path -> canonicalizePath path
    Nothing -> canonicalizePath defaultSourceDirectoryName
  outputPath <- case maybeOutputPath of
    Just path -> canonicalizePath path
    Nothing -> canonicalizePath defaultOutputDirectoryName
  return (sourcePath, outputPath)

data Command = 
    Build (Maybe FilePath) (Maybe FilePath) Bool Bool
  | Jekyll (Maybe FilePath) (Maybe FilePath) (Maybe String)

commandParser :: Parser Command
commandParser = subparser
  ( command "build" (info (buildParser <**> helper)
      ( fullDesc
     <> progDesc "Build the output directory from the source directory" ))
 <> command "jekyll" (info (jekyllParser <**> helper)
      ( fullDesc
     <> progDesc "Generate Jekyll-compatible posts from the source directory" ))
  )

buildParser :: Parser Command
buildParser = Build
  <$> optional (strOption
      ( long "source"
     <> metavar "SOURCE_DIR"
     <> help "Source directory" ))
  <*> optional (strOption
      ( long "output"
     <> metavar "OUTPUT_DIR"
     <> help "Output directory" ))
  <*> switch
      ( long "reset"
     <> help "Wipe the output directory (except assets/ dir) before building." )
  <*> switch
      ( long "dev-mode"
     <> help "Enable development mode (sets hostname to 'localhost' and disables user authentication)" )

jekyllParser :: Parser Command
jekyllParser = Jekyll
  <$> optional (strOption
      ( long "source"
     <> metavar "SOURCE_DIR"
     <> help "Source directory" ))
  <*> optional (strOption
      ( long "output"
     <> metavar "OUTPUT_DIR"
     <> help "Output directory for _posts" ))
  <*> optional (strOption
      ( long "after"
     <> metavar "DATE"
     <> help "Only process posts after the given date" ))

defaultEntryPoint :: IO ()
defaultEntryPoint = do
  command' <- execParser opts
  case command' of
    Build maybeSourcePath maybeOutputPath forceLocalhost reset -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      buildTree absoluteSourcePath absoluteOutputPath forceLocalhost reset
    Jekyll maybeSourcePath maybeOutputPath maybeAfter -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      library <- loadOnce absoluteSourcePath
      ToJekyll.buildTree library.config.server absoluteSourcePath absoluteOutputPath (pack <$> maybeAfter)
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Run the Bore CLI"
     <> header "bore - a CLI for serving and building gopherholes" )