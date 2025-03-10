module Bore.CLI (defaultEntryPoint) where

import Bore.FileLayout
import Bore.Library
import Bore.Parse (buildTree, applyDevMode)
import Bore.Config
import Bore.SpacecookieClone.Serve (runServerWithConfig)
import qualified Bore.ToJekyll as ToJekyll (buildTree)

import Data.Text (pack)
import System.Directory (canonicalizePath)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
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

{- | Serve the gopherhole, if there are any changes to the children of the "source path"
then rebuild the gopherhole.
-}
watchServe :: AbsolutePath -> AbsolutePath -> Bool -> IO ()
watchServe absoluteSourcePath absoluteOutputPath devMode = do
  putStrLn "Building first..."
  buildTree absoluteSourcePath absoluteOutputPath devMode

  putStrLn $ "Starting server and watching for changes in source: " ++ absoluteSourcePath

  -- Watch the directory for changes and rebuild on changes
  withManager $ \mgr -> do
    _ <- watchTree
      mgr
      absoluteSourcePath
      (const True) -- Watch all changes
      (\_ -> do
          putStrLn "Change detected, about to rebuild..."
          buildTree absoluteSourcePath absoluteOutputPath devMode)
    -- Keep the watcher alive
    library <- loadOnce absoluteSourcePath
    let modifiedConfig = applyDevMode library.config devMode
    runServerWithConfig modifiedConfig.server absoluteSourcePath absoluteOutputPath
    forever $ threadDelay 1000000

data Command = 
    WatchServe (Maybe FilePath) (Maybe FilePath) Bool
  | Build (Maybe FilePath) (Maybe FilePath) Bool
  | Jekyll (Maybe FilePath) (Maybe FilePath) (Maybe String)

commandParser :: Parser Command
commandParser = subparser
  ( command "watchServe" (info (watchServeParser <**> helper)
      ( fullDesc
     <> progDesc "Watch the source directory and serve the output directory" ))
 <> command "build" (info (buildParser <**> helper)
      ( fullDesc
     <> progDesc "Build the output directory from the source directory" ))
 <> command "jekyll" (info (jekyllParser <**> helper)
      ( fullDesc
     <> progDesc "Generate Jekyll-compatible posts from the source directory" ))
  )

watchServeParser :: Parser Command
watchServeParser = WatchServe
  <$> optional (strOption
      ( long "source"
     <> metavar "SOURCE_DIR"
     <> help "Source directory" ))
  <*> optional (strOption
      ( long "output"
     <> metavar "OUTPUT_DIR"
     <> help "Output directory which will also be served." ))
  <*> switch
      ( long "dev-mode"
     <> help "Enable development mode (sets hostname to 'localhost' and disables user authentication)" )

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
    WatchServe maybeSourcePath maybeOutputPath forceLocalhost -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      watchServe absoluteSourcePath absoluteOutputPath forceLocalhost
    Build maybeSourcePath maybeOutputPath forceLocalhost -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      buildTree absoluteSourcePath absoluteOutputPath forceLocalhost
    Jekyll maybeSourcePath maybeOutputPath maybeAfter -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      library <- loadOnce absoluteSourcePath
      ToJekyll.buildTree library.config.server absoluteSourcePath absoluteOutputPath (pack <$> maybeAfter)
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Run the Bore CLI"
     <> header "bore - a CLI for serving and building gopherholes" )