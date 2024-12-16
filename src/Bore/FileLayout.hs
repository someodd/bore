{- | Everything to do with file layout/structure of a project.Applicative

Tries to stick to the responsibility of being a central place to change file paths
and important file names.

Also includes tools for dealing with file paths and dealing with the file system,
rudimentary file operations that are only specific to project layout, in other words, do
things according to project layout rules, and nothing more specific.

Also handles some things to do with Gopher selectors.

-}

module Bore.FileLayout where

import Control.Monad (filterM, when, forM_)
import System.Directory (listDirectory, doesDirectoryExist, copyFile, createDirectoryIfMissing, canonicalizePath, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>), makeRelative, takeFileName, takeDirectory, isValid, isRelative, dropExtension)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.List (sortOn)
import Data.Ord (Down(..))
import Bore.Text.Clean (cleanText)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Text as T

-- | Filename for the default *parent* template to be used for all phlog posts, unless
-- overridden.
templatePostFileName :: RelativePath
templatePostFileName = "post.txt"

{- | Directory which "gets left alone" during the build process, i.e., it won't get
cleared out of output.

Relative to the OUTPUT directory.

-}
assetsDirectoryName :: RelativePath
assetsDirectoryName = "assets"

-- | Relative to the current working directory. This may get moved to FileLayout.
defaultSourceDirectoryName :: RelativePath
defaultSourceDirectoryName = "example"

-- | Relative to the current working directory. This may get moved to FileLayout.
defaultOutputDirectoryName :: RelativePath
defaultOutputDirectoryName = "output"

{- | Project configuration file.

Minimal rules for serving and building the project.

-}
projectConfigFile :: RelativePath
projectConfigFile = "bore.toml"

{- | Filename for bucktooth-style gophermaps, which work in spacecookie.

These files will simply be copied, but this name will also be used to create
menu indexes for a given directory, essentially.

-}
gophermapFileName :: FilePath
gophermapFileName = ".gophermap"

{- | This is similar to `gophermapFileName`, but it allows you to create
a *parseable* menu index gophermap for a given directory.

If this file is detected, the output file name will simply be `.gophermap`.

-}
parseableGophermapFileName :: FilePath
parseableGophermapFileName = ".parseable.gophermap"

{- | Hopefully an absolute `FilePath`.

-}
type AbsolutePath = FilePath

{- Hopefully a relative `FilePath`.

The purpose of this type is relative in the sense of a relative path that is logical in
both the output and source directories. Not necessarily relative to the current working
directory.

-}
type RelativePath = FilePath

{- | Directory where all phlog posts are kept and where phlog indexes are written.

-}
phlogDirectory :: RelativePath
phlogDirectory = "phlog/"

{- | Where tag indexes are written to.

Relative to the output directory and the phlog directory.

-}
tagIndexesDirectory :: RelativePath
tagIndexesDirectory = "tags/"

{- | Directory containing all the figlet fonts.

Relative path.

-}
figletFontDirectory :: RelativePath
figletFontDirectory = "fonts/"

{- | Path relative to the project root where all the container configs are stored.

-}
containersDirectory :: RelativePath
containersDirectory = "containers/"

{- | Only prase files with these file extensions.

-}
onlyParse :: [String]
onlyParse = [".gopher.txt", ".gopher.md", gophermapFileName]

{- | Relative file path to write the phlog index to.

Path will be joined with the destination directory.

-}
phlogIndexFileName :: FilePath
phlogIndexFileName = "phlog_index.txt"

{- | Relative file path/file name for the phlog atom feed.

-}
phlogFeedName :: RelativePath
phlogFeedName = "atom.xml"

-- | Path to includes/templates which can be used as partials and parents.
templateDirectory :: RelativePath
templateDirectory = "templates"

-- FIXME: get directories like templates and containers from their
-- respective modules?
-- THIS IS IGNORED FOR COPY AND EVEN PARSING MAYBE FOR NOW? FIXME
{- | Relative paths to match for ignoring files.

Assumed to be relative to the source directory root.

-}
ignorePaths :: [FilePath]
ignorePaths = [templateDirectory, containersDirectory, figletFontDirectory, projectConfigFile, projectConfigFile]

{- | Canonicalize all ignore paths to absolute paths.

-}
canonIgnorePaths :: AbsolutePath -> [FilePath] -> IO [FilePath]
canonIgnorePaths sourceDirectory = mapM (canonicalizePath . (sourceDirectory </>))

{- | Get all files recursively in a directory.

Skips over even relative directories.

Will skip/not return if any absolute path identical to an absolute path in ignore list.

@@absoluteIgnorePaths@@ is a list of absolute paths to ignore. Assumed to be canonicalized
relative to the source directory.
-}
getFilePathsRecursive :: [AbsolutePath] -> FilePath -> IO [AbsolutePath]
getFilePathsRecursive absoluteIgnorePaths dir = do
    directoryContents <- listDirectory dir
    directoryContentsAbsolute <- mapM canonicalizePath (map (dir </>) directoryContents)
    let paths = [name | name <- directoryContentsAbsolute, not (name `elem` absoluteIgnorePaths)]
    onlyFilesNoDirectories <- filterM doesDirectoryExist paths
    subDirs <- concat <$> mapM (getFilePathsRecursive absoluteIgnorePaths) onlyFilesNoDirectories
    return $ paths ++ subDirs

{- | Get the full extension of a file.

This function extracts the full extension from a given file path. 
It is particularly useful for files that use multiple extensions (e.g., .tar.gz).

Example usage:

>>> takeFullExtension "/path/to/file.tar.gz"
".tar.gz"

If the file has no extension, the function returns an empty string.

>>> takeFullExtension "/path/to/file"
""

This is because I like using .double.exts

You can also use it reliably for files that are "hidden files"
in this way:

>>> takeFullExtension $ "/path/to/" ++ ".parseable.gophermap"
".parseable.gophermap"

-}
takeFullExtension :: FilePath -> String
takeFullExtension path = 
    let fileName = takeFileName path
    in if '.' `elem` fileName then dropWhile (/= '.') fileName else ""

-- FIXME: is this just crud?
{- | Standaredized writing-to-destination.

Ensures directories created.

@@destinationDirectory@@: target directory being written to.

Not just for parseable files.

-}
writeDest :: FilePath -> AbsolutePath -> FilePath -> Text.Text -> IO (RelativePath, AbsolutePath)
writeDest sourceDirectory fullSourceFilePath outputDirectory fileContents = do
    (fullFileDestination, relativeFilePath) <- createDestinationDirectoryForFile outputDirectory sourceDirectory fullSourceFilePath
    TextIO.writeFile fullFileDestination fileContents
    pure (relativeFilePath, fullFileDestination)

createDestinationDirectoryForFile :: FilePath -> FilePath -> AbsolutePath -> IO (AbsolutePath, FilePath)
createDestinationDirectoryForFile outputDirectory sourceDirectory fullSourceFilePath = do
    let
        -- Make the relative path to the source file, so it works for both output and source directories
        relativeFilePath = makeRelative sourceDirectory fullSourceFilePath
        -- now make the relative directory path based on above
        relativeDirectoryPath = takeDirectory relativeFilePath
        -- now we can figure the full destination/output directory to copy the file to
        fullDirectoryDestination = outputDirectory </> relativeDirectoryPath
        -- finally we also want to know the full destination path for the file itself
        fullFileDestination = fullDirectoryDestination </> takeFileName fullSourceFilePath

    _ <- createDirectoryIfMissing True fullDirectoryDestination
    pure (fullFileDestination, relativeFilePath)

{- | Simply copy a file according to project layout rules.

@@fullSourceFilePath@@ is the full absolute path to the source file to be copied.

-}
onlyCopyFile :: FilePath -> FilePath -> AbsolutePath -> IO (FilePath, RelativePath)
onlyCopyFile sourceDirectory outputDirectory fullSourceFilePath = do
    (fullFileDestination, relativeFilePath) <- createDestinationDirectoryForFile outputDirectory sourceDirectory fullSourceFilePath
    copyFile fullSourceFilePath fullFileDestination
    pure (fullFileDestination, relativeFilePath)

{- | Clear out the output directory, but leave the assets directory alone.

Be sure to use an absolute path.

-}
resetOutputDirectory :: AbsolutePath -> IO ()
resetOutputDirectory outputDir = do
    -- List all items in the output directory
    contents <- listDirectory outputDir
    forM_ contents $ \item -> do
        let itemPath = outputDir </> item
        -- Delete the item if it is not the "assets" folder
        when (item /= assetsDirectoryName) $ do
            isDir <- doesDirectoryExist itemPath
            if isDir
                then removeDirectoryRecursive itemPath
                else removeFile itemPath

-- | Check if what's supposed to be a filename is safe and sane.
--
-- Example:
-- >>> isSafeFilename "file.txt"
-- True
-- >>> isSafeFilename "../file.txt"
-- False
-- >>> isSafeFilename "file.txt/"
-- False
-- >>> isSafeFilename "file.txt/.."
-- False
-- >>> isSafeFilename "."
-- False
-- >>> isSafeFilename "/dir/file.txt"
-- False
-- >>> isSafeFilename "file"
-- True
isSafeFilename :: FilePath -> Bool
isSafeFilename name =
    isValid name              -- Checks if it contains only valid characters
        && not (null name)        -- Ensures it’s not empty
        && not (null $ takeFileName name)  -- Ensures it’s not just a directory
        && takeFileName name == name -- ensure it doesn't have a directory traversal
        && isRelative name     -- Ensures it's a relative path
        && name /= "." && name /= ".."  -- Disallows "." and ".."
        && filenameByteLength name <= 255  -- Ensures it's not too long

-- | Get the words/tokens from a selector, and normalize them.
--
-- Example:
-- >>> selectorWords "hello_world-foo bar"
-- ["hello","world","foo","bar"]
selectorWords :: FilePath -> [Text.Text]
selectorWords = pathWords . Text.toLower . cleanText . Text.pack

-- | Get the byte length of a filename in UTF-8 encoding
filenameByteLength :: String -> Int
filenameByteLength filename = B.length . encodeUtf8 $ T.pack filename

-- | Get the words/tokens from a path.
--
-- Also enforces a maximum length.
--
-- Differs from `selectorWords` in that it's not focused on cleaning the text of any
-- ineappropriate characters.
--
-- Example:
-- >>> pathWords "hello_world-foo bar"
-- ["hello","world","foo","bar"]
-- >>> pathWords "hello/word/foo.gopher.txt"
-- ["hello","word","foo"]
pathWords :: Text.Text -> [Text.Text]
pathWords = Text.split (`elem` splitChars) . dropExtensionSpecial
  where
    splitChars = [' ', '_', '-', '.', '/']
    sortedExtensions = sortOn (Down . Text.length) (map Text.pack onlyParse)
    -- Drop the onlyParse extensions if it exists, otherwise just use dropExtension from FilePath
    dropExtensionSpecial :: Text.Text -> Text.Text
    dropExtensionSpecial fp = 
        let
          fpNoExt = dropExtension (Text.unpack fp)  -- Remove last extension if no specific match
        in
          case filter (`Text.isSuffixOf` fp) sortedExtensions of
            (ext:_) -> Text.dropEnd (Text.length ext) fp
            []      -> Text.pack fpNoExt