{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Bore.ToJekyll (buildTree) where

import Bore.FrontMatter
import Bore.Config (ServerConfig(..))
import Bore.FileLayout (phlogDirectory)

import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import System.Directory (canonicalizePath, createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, combine, makeRelative, (</>))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist)
import Data.Aeson (Value(..))
import Data.Traversable (forM)
import qualified Data.Aeson.KeyMap as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Aeson.Key (fromText)

-- | Parse a file into a Jekyll-compatible blog post.
parseToJekyll :: ServerConfig -> FilePath -> FilePath -> FilePath -> IO ()
parseToJekyll config sourceDir destDir filePath = do
    content <- TextIO.readFile filePath
    let relativePath = makeRelative sourceDir filePath
        frontMatterAndRest = parseFrontMatterIgnoreErrors content

    case frontMatterAndRest of
        Just (frontMatter, body) -> do
            if shouldSkipJekyll frontMatter || fromMaybe False (draft frontMatter)
                then putStrLn $ "Skipping file (skipJekyll or draft is True): " ++ filePath
                else do
                    let fmMap = prepareJekyllFrontMatter frontMatter
                        filename = makeJekyllFilename relativePath fmMap
                        outputPath = combine destDir filename
                        frontMatterYaml = Text.decodeUtf8 . Yaml.encode . Object $ fmMap
                        finalContent = renderJekyllPost config frontMatterYaml body
                    createDirectoryIfMissing True destDir
                    TextIO.writeFile outputPath finalContent
        Nothing -> putStrLn $ "Skipping file (no valid frontmatter): " ++ filePath

-- | Check if the file should be skipped for Jekyll output.
shouldSkipJekyll :: FrontMatter -> Bool
shouldSkipJekyll fm = fromMaybe False (skipJekyll fm)

-- | Prepare the Jekyll frontmatter as a Map for YAML export.
prepareJekyllFrontMatter :: FrontMatter -> Map.KeyMap Value
prepareJekyllFrontMatter fm =
    let jekyllFm = case jekyll fm of
            Just (Object obj) -> obj
            _                 -> mempty
        fallbackTitle = String $ fromMaybe "Untitled" (title fm)
        fallbackDate = String $ fromMaybe "1970-01-01" (date fm)
        fallbackTags = Array . V.fromList . fmap String $ fromMaybe [] (tags fm)
    in jekyllFm <> Map.fromList
        [ (fromText "title", fallbackTitle)
        , (fromText "date", fallbackDate)
        , (fromText "tags", fallbackTags)
        ]

-- | Create a Jekyll-compatible filename.
makeJekyllFilename :: FilePath -> Map.KeyMap Value -> FilePath
makeJekyllFilename relativePath fm =
    let baseName = takeBaseName relativePath
        fmDate = case Map.lookup "date" fm of
            Just (String date) -> Text.unpack date
            _                  -> "1970-01-01"
        sanitizedTitle = Text.unpack . Text.replace " " "-" . Text.toLower . Text.pack $ baseName
    in fmDate ++ "-" ++ sanitizedTitle ++ ".md"

-- | Render the final Jekyll post with frontmatter and body.
renderJekyllPost :: ServerConfig -> Text.Text -> Text.Text -> Text.Text
renderJekyllPost config frontMatter body = do
    let
        uri = "gopher://" <> config.hostname <> ":" <> Text.pack (show config.listenPort) <> "/" <> Text.pack phlogDirectory
    Text.unlines
        [ "---"
        , frontMatter
        , "---"
        , "Original content in gopherspace: " <> uri
        , body
        ]

-- | Process all files in the source directory.
processFiles :: ServerConfig -> FilePath -> FilePath -> [FilePath] -> IO ()
processFiles config sourceDir destDir files = do
    forM_ files $ \file -> do
        isDir <- doesDirectoryExist file
        if not isDir && takeExtension file `elem` [".txt", ".md"]
            then parseToJekyll config sourceDir destDir file
            else putStrLn $ "Skipping directory or unsupported file: " ++ file

-- | Main function to build the Jekyll-compatible blog.
buildTree :: ServerConfig -> FilePath -> FilePath -> IO ()
buildTree config sourceDir destDir = do
    sourceDirAbs <- canonicalizePath (sourceDir </> phlogDirectory)
    destDirAbs <- canonicalizePath (combine destDir "_posts")
    files <- listDirectoryRecursive sourceDirAbs
    processFiles config sourceDirAbs destDirAbs files

-- | Recursively list all files in a directory.
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    entries <- listDirectory dir
    paths <- forM entries $ \entry -> do
        let fullPath = combine dir entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then listDirectoryRecursive fullPath
            else return [fullPath]
    return (concat paths)
