{- | Entrypoints for the CLI.

-}

module Bore.CLI (defaultEntryPoint) where

import Bore.FileLayout
import Bore.Library
import Bore.Parse (buildTree)
import Bore.Config
import Bore.SpacecookieClone.Serve (runServerWithConfig)

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
watchServe :: AbsolutePath -> AbsolutePath -> IO ()
watchServe absoluteSourcePath absoluteOutputPath = do
  putStrLn "Building first..."
  buildTree absoluteSourcePath absoluteOutputPath

  putStrLn $ "Starting server and watching for changes in source: " ++ absoluteSourcePath

  -- Watch the directory for changes and rebuild on changes
  withManager $ \mgr -> do
    _ <- watchTree
      mgr
      absoluteSourcePath
      (const True) -- Watch all changes
      (\_ -> do
          putStrLn "Change detected, about to rebuild..."
          buildTree absoluteSourcePath absoluteOutputPath)
    -- Keep the watcher alive
    --changeWorkingDirectory projectRootPath
    library <- loadOnce absoluteSourcePath
    -- modify the config to use the output directory as the root. this could just be the config value if it was not overridden in the cli
    runServerWithConfig library.config.server absoluteSourcePath absoluteOutputPath
    forever $ threadDelay 1000000

data Command = WatchServe (Maybe FilePath) (Maybe FilePath) | Build (Maybe FilePath) (Maybe FilePath)

commandParser :: Parser Command
commandParser = subparser
  ( command "watchServe" (info (watchServeParser <**> helper)
      ( fullDesc
     <> progDesc "Watch the source directory and serve the output directory" ))
 <> command "build" (info (buildParser <**> helper)
      ( fullDesc
     <> progDesc "Build the output directory from the source directory" ))
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

defaultEntryPoint :: IO ()
defaultEntryPoint = do
  command' <- execParser opts
  case command' of
    WatchServe maybeSourcePath maybeOutputPath -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      watchServe absoluteSourcePath absoluteOutputPath
    Build maybeSourcePath maybeOutputPath -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      buildTree absoluteSourcePath absoluteOutputPath
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Run the Bore CLI"
     <> header "bore - a CLI for serving and building gopherholes" )
