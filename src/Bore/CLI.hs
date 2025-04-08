module Bore.CLI (defaultEntryPoint) where

import Venusia.Systemd

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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Options.Applicative
import Data.Maybe (fromMaybe)
import System.Environment (getExecutablePath)
import qualified Data.Text as T

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
watchServe :: AbsolutePath -> AbsolutePath -> Bool -> Int -> IO ()
watchServe absoluteSourcePath absoluteOutputPath devMode waitSeconds = do
  putStrLn "Building first..."
  buildTree absoluteSourcePath absoluteOutputPath devMode

  putStrLn $ "Starting server and watching for changes in source: " ++ absoluteSourcePath
  putStrLn $ "Will wait " ++ show waitSeconds ++ " seconds after detecting changes before rebuilding"

  -- Create a mutable reference to track when the last rebuild was triggered
  lastRebuildVar <- newIORef =<< getCurrentTime
  
  -- Watch the directory for changes and rebuild on changes
  withManager $ \mgr -> do
    _ <- watchTree
      mgr
      absoluteSourcePath
      (const True) -- Watch all changes
      (\_ -> do
          -- Check if enough time has passed since the last rebuild
          now <- getCurrentTime
          lastRebuild <- readIORef lastRebuildVar
          let timeSinceLastRebuild = diffUTCTime now lastRebuild
          let secondsSinceLastRebuild = floor (realToFrac timeSinceLastRebuild :: Double)
          
          if secondsSinceLastRebuild < waitSeconds
            then putStrLn $ "Change detected, but ignoring as previous rebuild was " ++ 
                           show secondsSinceLastRebuild ++ " seconds ago"
            else do
              putStrLn $ "Change detected, waiting " ++ show waitSeconds ++ " seconds before rebuilding..."
              threadDelay (waitSeconds * 1000000)  -- Convert seconds to microseconds
              
              -- Check if another rebuild has been triggered during our wait
              writeIORef lastRebuildVar =<< getCurrentTime
              putStrLn "Rebuilding now..."
              buildTree absoluteSourcePath absoluteOutputPath devMode)
    -- Keep the watcher alive
    library <- loadOnce absoluteSourcePath
    let modifiedConfig = applyDevMode library.config devMode
    runServerWithConfig modifiedConfig.server absoluteSourcePath absoluteOutputPath
    forever $ threadDelay 1000000

data Command = 
    WatchServe (Maybe FilePath) (Maybe FilePath) Bool Int
  | Build (Maybe FilePath) (Maybe FilePath) Bool
  | Jekyll (Maybe FilePath) (Maybe FilePath) (Maybe String)
  | Systemd (Maybe FilePath) (Maybe FilePath)

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
 <> command "systemd" (info (systemdParser <**> helper)
      ( fullDesc
     <> progDesc "Create a systemd service file" ))
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
  <*> option auto
      ( long "wait"
     <> metavar "SECONDS"
     <> help "Number of seconds to wait before rebuilding after a change is detected"
     <> value 0
     <> showDefault )

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

systemdParser :: Parser Command
systemdParser = Systemd
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
    WatchServe maybeSourcePath maybeOutputPath forceLocalhost waitSeconds -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      watchServe absoluteSourcePath absoluteOutputPath forceLocalhost waitSeconds
    Build maybeSourcePath maybeOutputPath forceLocalhost -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      buildTree absoluteSourcePath absoluteOutputPath forceLocalhost
    Jekyll maybeSourcePath maybeOutputPath maybeAfter -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      library <- loadOnce absoluteSourcePath
      ToJekyll.buildTree library.config.server absoluteSourcePath absoluteOutputPath (pack <$> maybeAfter)
    Systemd maybeSourcePath maybeOutputPath -> do
      (absoluteSourcePath, absoluteOutputPath) <- determineDirectories maybeSourcePath maybeOutputPath
      library <- loadOnce absoluteSourcePath
      let
        port = (fromIntegral $ fromMaybe 70 library.config.server.listenPort :: Int)
        user = T.unpack <$> library.config.server.user
        execArgs = "watchServe --wait 10 --source " ++ absoluteSourcePath ++ " --output " ++ absoluteOutputPath
      -- Get the path to our own executable
      exePath <- getExecutablePath
      let fullExePath = exePath ++ " " ++ execArgs
      -- Setup the systemd service
      -- FIXME: this stuff is hardcoded for now
      setupSystemdService 
        "bore"         -- Service name
        fullExePath    -- Executable path
        (show port)    -- Port
        "bore"           -- User
        "bore"        -- Group
        (Just absoluteSourcePath)  -- Working directory
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Run the Bore CLI"
     <> header "bore - a CLI for serving and building gopherholes" )