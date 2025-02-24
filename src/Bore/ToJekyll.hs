{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Bore.ToJekyll (buildTree) where

import Bore.FrontMatter
import Bore.Config (ServerConfig(..))
import Bore.FileLayout (phlogDirectory)
import Bore.Text.Gophermap (gopherFileTypes)

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
import Text.Regex.TDFA.Text ()
import Text.Regex.TDFA ((=~))

{-| Detect and replace Gophermap links with Markdown links.

This also detects the bucktooth shorthand gophermap links.

Performs a simple regex find and replace. Searches for an entire line which begins with a
character from `gopherFileTypes` and contains some text, and then a tab and then a path
(for a relative path [assume the host/port passed]), but if there's more than one tab then
it will expect the host and port according to gopher spec.

This also avoids detection that may occur inside of code blocks.
-}
replaceGophermapLinks :: Bool -> ServerConfig -> Text.Text -> Text.Text
replaceGophermapLinks overridePort config input =
  let
    hostForRelativeLinks = hostname config
    portForRelativeLinks = if overridePort then 70 else fromMaybe 70 (listenPort config)
    -- For full gophermap lines:
    --   Group1: file type
    --   Group2: display text (everything up to a tab)
    --   Group3: selector (must be non-tab characters)
    --   Group4: optional entire host/port part (unused)
    --   Group5: host (if provided)
    --   Group6: port (if provided)
    pattern1 :: Text.Text
    pattern1 = "^([" <> Text.pack gopherFileTypes <> "])([^\t]*)\t([^\t]+)(\t([^\t]+)\t([^\t]+))?"
    -- For bucktooth shorthand:
    --   Group1: file type (immediately after '>')
    --   Group2: display text
    --   Group3: selector
    --   Group4: optional entire host/port part (unused)
    --   Group5: host (if provided)
    --   Group6: port (if provided)
    pattern2 :: Text.Text
    pattern2 = "^>(.)([^\t]*)\t([^\t]+)(\t([^\t]+)\t([^\t]+))?"

    -- Build the URI using the file type as the first directory.
    buildURI fileType selector host port =
      let sep = if Text.isPrefixOf "/" selector then "" else "/"
      in Text.concat ["gopher://", host, ":", port, "/", fileType, sep, selector]

    processPattern1 groups =
      let fileType    = groups !! 0
          displayText = groups !! 1
          selector    = groups !! 2
          host        = if length groups > 4 && not (Text.null (groups !! 4))
                          then groups !! 4
                          else hostForRelativeLinks
          port        = if length groups > 5 && not (Text.null (groups !! 5))
                          then groups !! 5
                          else Text.pack (show portForRelativeLinks)
          url         = buildURI fileType selector host port
      in Text.concat ["[", displayText, "](", url, ")"]

    processPattern2 groups =
      let fileType    = groups !! 0
          displayText = groups !! 1
          selector    = groups !! 2
          host        = if length groups > 4 && not (Text.null (groups !! 4))
                          then groups !! 4
                          else hostForRelativeLinks
          port        = if length groups > 5 && not (Text.null (groups !! 5))
                          then groups !! 5
                          else Text.pack (show portForRelativeLinks)
          url         = buildURI fileType selector host port
      in Text.concat ["[", displayText, "](", url, ")"]

    replaceLine line =
      let (_ :: Text.Text, matched1 :: Text.Text, _ :: Text.Text, groups1 :: [Text.Text]) = line =~ pattern1
      in if not (Text.null matched1)
           then processPattern1 groups1
           else let (_ :: Text.Text, matched2 :: Text.Text, _ :: Text.Text, groups2 :: [Text.Text]) = line =~ pattern2
                in if not (Text.null matched2)
                     then processPattern2 groups2
                     else line

    -- Determines if a line starts a code fence.
    isFence line = "```" `Text.isPrefixOf` Text.strip line

    -- Process lines while tracking whether we're inside a code block.
    go _ [] = []
    go inCodeBlock (l:ls)
      | isFence l = l : go (not inCodeBlock) ls
      | inCodeBlock = l : go inCodeBlock ls
      | otherwise = replaceLine l : go inCodeBlock ls

  in Text.unlines (go False (Text.lines input))

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
        port = fromMaybe 70 (config.listenPort)
        uri = "gopher://" <> config.hostname <> ":" <> Text.pack (show port) <> "/" <> Text.pack phlogDirectory
    Text.unlines
        [ "---"
        , frontMatter
        , "---"
        -- FIXME: the True here is a hack to set the relative links to port 70 for now,
        -- because I have no way of overriding relative link paths with a setting in case
        -- the port listened on and the actual port we want for internal gophermap links
        -- differs (like if I use a reverse proxy)
        , replaceGophermapLinks True config body
        , "Original content in gopherspace: " <> uri
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
