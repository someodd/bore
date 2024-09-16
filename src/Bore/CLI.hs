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

defaultEntryPoint :: IO ()
defaultEntryPoint = watchServe

{- | Serve the gopherhole, if there are any changes to the children of the "source path"
then rebuild the gopherhole.

-}
watchServe :: IO ()
watchServe = do
  absoluteSourcePath <- canonicalizePath defaultSourceDirectoryName
  absoluteOutputPath <- canonicalizePath defaultOutputDirectoryName

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
    runServerWithConfig library.config.server
    forever $ threadDelay 1000000