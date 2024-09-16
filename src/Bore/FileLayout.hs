{- | Everything to do with file layout/structure of a project.Applicative

Tries to stick to the responsibility of being a central place to change file paths
and important file names.

Also includes tools for dealing with file paths and dealing with the file system,
rudimentary file operations that are only specific to project layout, in other words, do
things according to project layout rules, and nothing more specific.

-}

module Bore.FileLayout where

import Control.Monad (filterM)
import System.Directory (listDirectory, doesDirectoryExist, copyFile, createDirectoryIfMissing, canonicalizePath)
import System.FilePath ((</>), makeRelative, takeFileName, takeDirectory)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

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

{- | Directory to keep phlog index files in.

-}
phlogIndexDirectory :: RelativePath
phlogIndexDirectory = "phlog_indexes/"

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
onlyParse = [".gopher.txt", ".gopher.md", parseableGophermapFileName]

{- | Relative file path to write the phlog index to.

Path will be joined with the destination directory.

-}
phlogIndexFileName :: FilePath
phlogIndexFileName = "phlog_index.txt"

{- | Relative file path/file name for the phlog atom feed.

-}
phlogFeedName :: RelativePath
phlogFeedName = "atom.xml"

-- FIXME: get directories like templates and containers from their
-- respective modules?
-- THIS IS IGNORED FOR COPY AND EVEN PARSING MAYBE FOR NOW? FIXME
{- | Relative paths to match for ignoring files.

Assumed to be relative to the source directory root.

-}
ignorePaths :: [FilePath]
ignorePaths = ["templates", containersDirectory, figletFontDirectory, projectConfigFile, projectConfigFile]

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

>>> takeFullExtension $ "/path/to/" ++ parseableGophermapFileName
".parseable.gophermap"

-}
takeFullExtension :: FilePath -> String
takeFullExtension path = 
    let fileName = takeFileName path
    in if '.' `elem` fileName then dropWhile (/= '.') fileName else ""

{- | Create a destination directory.

@outputDirectory@ is the full absolute path to the output directory.

-}
createDestinationDirectory
    :: FilePath
    -> FilePath
    -> FilePath
    -> IO FilePath
createDestinationDirectory projectDirectory outputDirectory sourceFilePath = do
    
    let fullDestination = outputDirectory </> makeRelative projectDirectory (projectDirectory </> sourceFilePath)
    createDirectoryIfMissing True fullDestination
    pure fullDestination

{- | Standaredized writing-to-destination.

"Destination" here being the target directory being written to. Ensures directories
created.

Not just for parseable files.

Will also make a decision about the actual destination path to write to, namely
if the file is a `parseableGophermapFileName`, to write it to a `gophermapFileName`.

-}
writeDest :: FilePath -> FilePath -> FilePath -> Text.Text -> IO (RelativePath, AbsolutePath)
writeDest projectDirectory sourceFilePath destinationDirectory fileContents = do
    let
        relativePath = makeRelative projectDirectory sourceFilePath
        fullDestination = destinationDirectory </> relativePath
        fullDestinationDir = takeDirectory fullDestination

    -- Create the directory if it does not exist
    createDirectoryIfMissing True fullDestinationDir

    -- Handle special case where we change the write out file name in the event
    -- of handling `parseableGophermapFileName`.
    let finalDestination = if takeFileName sourceFilePath == parseableGophermapFileName
                           then destinationDirectory </> gophermapFileName
                           else fullDestination

    TextIO.writeFile finalDestination fileContents
    pure (relativePath, finalDestination)

{- | Simply copy a file according to project layout rules.

-}
onlyCopyFile :: FilePath -> FilePath -> FilePath -> IO (FilePath, RelativePath)
onlyCopyFile projectDirectory destination filePath = do
    fullTargetDirectory <- createDestinationDirectory projectDirectory destination filePath
    let fullTargetPath = fullTargetDirectory </> takeFileName filePath
    copyFile filePath fullTargetPath
    let relativePath = makeRelative fullTargetDirectory fullTargetPath
    pure (fullTargetPath, relativePath)
