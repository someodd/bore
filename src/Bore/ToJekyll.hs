{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Bore.ToJekyll (buildTree) where

import Bore.FrontMatter
import Bore.Config (ServerConfig(..))
import Bore.FileLayout (phlogDirectory)
import Bore.Text.Gophermap (gopherFileTypes)

import Data.Dates.Parsing (parseDate, defaultConfig, DateTime)
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import System.Directory
    ( canonicalizePath,
      createDirectoryIfMissing,
      listDirectory,
      copyFile,
      doesDirectoryExist,
      doesFileExist )
import System.FilePath (takeBaseName, takeExtension, combine, makeRelative, (</>), takeDirectory)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..))
import Data.Traversable (forM)
import qualified Data.Aeson.KeyMap as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Aeson.Key (fromText)
import Text.Regex.TDFA.Text ()
import Text.Regex.TDFA ((=~), AllTextMatches (getAllTextMatches))
import qualified Data.List as List

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
    portForRelativeLinks = if overridePort then 70 else fromMaybe 70 config.hostPort
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

{- | Get all of the file paths for assets in a post.

Any filepath found that begins with /assets/ is considered an asset.

Must work for:
- YAML frontmatter:  path: /assets/...
- Markdown images:   ![](/assets/...)
- HTML:              <img src="/assets/...">

Does NOT rewrite the body. Whatever is referenced as /assets/ stays /assets/.
-}
extractAssets :: Text.Text -> (Text.Text, [Text.Text])
extractAssets body =
  let
    -- Match /assets/... until a terminator.
    pat :: Text.Text
    pat = "/assets/[^][:space:])\"']+"

    matches :: [Text.Text]
    matches = getAllTextMatches (body =~ pat)

    -- de-dupe, keep body unchanged (per your requirement)
    assets = List.nub matches
  in
    (body, assets)

{- | Copy assets from the source directory to the destination directory.

Don't worry/skip assets that already exist in destination or don't exist in source.

Copies to destination `/assets/phlog/`.

-}
copyAssets :: FilePath -> FilePath -> [Text.Text] -> IO ()
copyAssets destDir assetsDir assets = do
    forM_ assets $ \asset -> do
        let sourcePath = combine assetsDir (makeRelative "/assets" $ Text.unpack asset)
            newAssetDestPath = destDir </> "assets" </> makeRelative "/assets" (Text.unpack asset)
        -- ensure the destination directory exists, including children directories
        createDirectoryIfMissing True (takeDirectory newAssetDestPath)

        sourceFileExists <- doesFileExist sourcePath
        if not sourceFileExists
            then putStrLn $ "Asset not found, skipping: " ++ sourcePath
            else do
                putStrLn $ "Copying asset: " ++ sourcePath ++ " to " ++ newAssetDestPath
                copyFile sourcePath newAssetDestPath

-- | Parse a file into a Jekyll-compatible blog post.
parseToJekyll
  :: ServerConfig
  -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath
  -> Maybe Text.Text
  -> Bool
  -> Maybe FilePath
  -> IO ()
parseToJekyll config sourceDir postSourceDir destDir postDestDirRelative filePath maybeAfter skipAssets maybeAssetsDir = do
    content <- TextIO.readFile filePath

    assetReplacedContent <-
        if skipAssets
            then
                pure content
            else do
                -- Extract assets and copy them to the destination directory. A little magic to make
                -- having multimedia and downloads in a phlog post, porting over to Jekyll, all the
                -- eaiser.
                let (assetReplacedContent, assetsToCopy) = extractAssets content
                    assetsDir = fromMaybe (sourceDir </> "assets") maybeAssetsDir
                putStrLn $ "extractAssets found " ++ show (length assetsToCopy) ++ " assets in " ++ filePath
                forM_ assetsToCopy $ \a -> putStrLn $ "  asset: " ++ Text.unpack a

                copyAssets destDir assetsDir assetsToCopy
                pure assetReplacedContent

    let relativePath = makeRelative postSourceDir filePath
        frontMatterAndRest = parseFrontMatterIgnoreErrors assetReplacedContent
    -- Define a reference date for parsing, here we use the epoch.
    let refDate = read "1970-01-01 00:00:00" :: DateTime
        parseWithDefault s = either (const Nothing) Just (parseDate (defaultConfig refDate) s)
    case frontMatterAndRest of
        Just (frontMatter, body) -> do
            if shouldSkipJekyll frontMatter || fromMaybe False (draft frontMatter)
                then putStrLn $ "Skipping file (skipJekyll or draft is True): " ++ filePath
                else do
                  case maybeAfter of
                    Just afterText ->
                      let maybeAfterDate = parseWithDefault (Text.unpack afterText)
                          maybeFmDate = date frontMatter >>= (\d -> parseWithDefault (Text.unpack d))
                      in case (maybeAfterDate, maybeFmDate) of
                           (Just afterDate, Just fmDate) ->
                             if fmDate > afterDate
                               then processFile
                               else putStrLn $ "Skipping file (date is not after " ++ Text.unpack afterText ++ "): " ++ filePath
                           _ -> putStrLn $ "Skipping file (unable to parse dates): " ++ filePath
                    Nothing -> processFile
          where
            processFile = do
              let fmMap = prepareJekyllFrontMatter frontMatter
                  filename = makeJekyllFilename relativePath fmMap
                  outputPath = combine (destDir </> postDestDirRelative) filename
                  frontMatterYaml = Text.decodeUtf8 . Yaml.encode . Object $ fmMap
                  finalContent = renderJekyllPost (takeBaseName relativePath) config frontMatter frontMatterYaml body
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
renderJekyllPost :: FilePath -> ServerConfig -> FrontMatter -> Text.Text -> Text.Text -> Text.Text
renderJekyllPost postFileName config frontMatter frontMatterText body = do
    let
        overridePort = True  -- Making room for a future update where this is more controllable. also matches the regex link replacing action
        port = if overridePort then 70 else fromMaybe 70 config.hostPort
        number = if fromMaybe False frontMatter.gophermap then "1" else "0"
        uri = "gopher://" <> config.hostname <> ":" <> Text.pack (show port) <> "/" <> number <> "/" <> Text.pack phlogDirectory <> Text.pack postFileName <> ".txt"
    Text.unlines
        [ "---"
        , frontMatterText
        , "---"
        -- FIXME: the True here is a hack to set the relative links to port 70 for now,
        -- because I have no way of overriding relative link paths with a setting in case
        -- the port listened on and the actual port we want for internal gophermap links
        -- differs (like if I use a reverse proxy)
        , replaceGophermapLinks overridePort config body
        , "Original content in gopherspace: [" <> uri <> "](" <> uri <> ")"
        ]

-- | Process all files in the source directory.
processFiles
  :: ServerConfig
  -> FilePath -> FilePath -> FilePath -> FilePath
  -> [FilePath]
  -> Maybe Text.Text
  -> Bool
  -> Maybe FilePath
  -> IO ()
processFiles config sourceDir postSourceDir destDir postDestDirRelative files maybeAfter skipAssets maybeAssetsDir = do
    forM_ files $ \file -> do
        isDir <- doesDirectoryExist file
        if not isDir && takeExtension file `elem` [".txt", ".md"]
            then parseToJekyll config sourceDir postSourceDir destDir postDestDirRelative file maybeAfter skipAssets maybeAssetsDir
            else putStrLn $ "Skipping directory or unsupported file: " ++ file

-- | Main function to build the Jekyll-compatible blog.
buildTree :: ServerConfig -> FilePath -> FilePath -> Maybe Text.Text -> Bool -> Maybe FilePath -> IO ()
buildTree config sourceDir destDir maybeAfter skipAssets maybeAssetsDir = do
    sourceDirAbs <- canonicalizePath sourceDir
    postSourceDirAbs <- canonicalizePath (sourceDir </> phlogDirectory)
    destDirAbs <- canonicalizePath destDir
    let postDestDirRelative = "phlog-mirror" </> "_posts"
    files <- listDirectoryRecursive postSourceDirAbs
    processFiles config sourceDirAbs postSourceDirAbs destDirAbs postDestDirRelative files maybeAfter skipAssets maybeAssetsDir

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
