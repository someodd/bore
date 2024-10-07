{- | Main high-level logic for building/parsing through a directory tree.

over files in a directory to parse to an output directory.

errors are being supressed in some areas i think

could encounter problem if a directory has both a .gophermap AND a .parseable.gophermap

I should add wordwrap functionality for some stuff? Maybe just for markdown eventually?
Render settings in frontmatter too? I guess containers help with that too.

render: render the parsed files to the output directory
parse: operate to transform text to text

Always deal with absolute `FilePath` except when explicitly needing relative paths.

FIXME

ignore not working!

overwriting output directory

PROBLEM

how should i handle transforming into a gophermap? will spacecookie automatically parse
without the tabs for hostname and stuff? should I assume it's all like that?

Transform markdown images, links into gophermap links if value set in frontmatter--
mdGopherItemTypes=true? Because of the flexibility of the bucktooth gophermap format, I
don't think specially handling generation of gophermaps is necessary. Mostly rely on the
user knowing some gophermap syntax for when they want to link to content.

ANOTHER ISSUE

how do i handle creating .gophermaps? just by filename i guess? i think that's the best. just blindly copy?
any situations where i may want it?

STANDARDIZE TERMS

build directory: where everything gets outputted to.
-}

-- NOW ADD ALL THE TESTS.
module Bore.Parse (buildTree) where

import Bore.FrontMatter
import Bore.Template (parseTemplate)
import Bore.Library
import Bore.Phlog
import Bore.FileLayout
import Bore.Text.Gophermap
import Bore.Config

import qualified Data.Text.IO as TextIO
import System.Directory (canonicalizePath)
import Data.Maybe (fromMaybe, catMaybes)

import System.Directory (doesDirectoryExist)

-- FIXME: TODO: add markdown?
{- | Parse a file, returning the full destination path, the relative destination path, and
(maybe) the frontmatter.

-}
parseFile :: Library -> FilePath -> FilePath -> FilePath -> IO (FilePath, RelativePath, Maybe FrontMatter)
parseFile library projectDirectory sourceFile destination = do
    fileContents <- TextIO.readFile sourceFile
    let
        -- FIXME: suppressing errors
        frontMatterAndRestMaybe = either (const Nothing) Just (parseFrontMatter fileContents)
        frontMatterMaybe = fst <$> frontMatterAndRestMaybe
        fileContentsStripped = snd <$> frontMatterAndRestMaybe

    afterTemplatingText <- possiblyTemplate frontMatterMaybe fileContents fileContentsStripped
    
    -- Probably important that turning it into a gopher menu is the last step.
    let possiblyGopherizedText = possiblyGopherize afterTemplatingText frontMatterMaybe

    (relativePath, fullDestination) <- writeDest projectDirectory sourceFile destination possiblyGopherizedText
    pure (fullDestination, relativePath, frontMatterMaybe)
  where
    possiblyTemplate frontMatterMaybe fileContents fileContentsStripped = case frontMatterMaybe of
        Just frontMatter -> do
            if fromMaybe False frontMatter.skipTemplating
                then pure fileContents
                else do
                    parseTemplate library projectDirectory (Just frontMatter) (fromMaybe "" fileContentsStripped)
        Nothing -> pure fileContents

    possiblyGopherize someText maybeFrontMatter = do
        case maybeFrontMatter of
            Just frontMatter -> do
                if isGophermap frontMatter
                    then
                        let
                            hostname = library.config.server.hostname
                            port = fromMaybe 70 (fromIntegral <$> library.config.server.listenPort)
                        in
                            toGophermap hostname port someText
                    else
                        someText
            Nothing -> someText


{- | Function which handles a file in the walking process.

Focused on side-effects (writing).

If it's a directory, the return is nothing.

Changes the output destination if the file is a `parseableGophermapFileName`.

@@destination@@ is the full absolute path to the output directory.
@@filePath@@ absolute path to the file being handled. Should be absolute path to the source file.

-}
handleFile :: Library -> FilePath -> FilePath -> FilePath -> IO (Maybe (FilePath, FilePath, Maybe FrontMatter))
handleFile library projectDirectory destination filePath = do
    -- It's important to check if it's a directory first, because we don't want to match
    -- directories as parseable files just because they have a file extension we are
    -- looking for.
    isDir <- doesDirectoryExist filePath
    if isDir
        then do
            putStrLn $ "creating directory: " ++ filePath
            createDestinationDirectory projectDirectory destination filePath >> pure Nothing
        else if takeFullExtension filePath `elem` onlyParse
            then do
                putStrLn $ "parse the file: " ++ filePath
                Just <$> parseFile library projectDirectory filePath destination
            else do
                putStrLn $ "only copy file: " ++ filePath
                (fullPath, relativePath) <- onlyCopyFile projectDirectory destination filePath
                pure $ Just (fullPath, relativePath, Nothing)

{- | Helper function to get all the handle all files passed, returning metadata regarding
the files which were actually copied.

Also handles the creation of directories.

-}
handleAllFiles :: Library -> FilePath -> FilePath -> [FilePath] -> IO [(FilePath, FilePath, Maybe FrontMatter)]
handleAllFiles library projectDirectoryAbsolutePath outputDirectoryAbsolutePath filesToCopy =
    catMaybes <$> mapM (handleFile library projectDirectoryAbsolutePath outputDirectoryAbsolutePath) filesToCopy

{- | Main entrypoint to build the entire gopherhole project.

The file paths can be absolute or relative, they will be made absolute.

-}
buildTree :: FilePath -> FilePath -> IO ()
buildTree sourceDirectory outputDirectory = do
    sourceDirectoryAbsolutePath <- canonicalizePath sourceDirectory
    outputDirectoryAbsolutePath <- canonicalizePath outputDirectory

    _ <- resetOutputDirectory outputDirectoryAbsolutePath

    library <- loadOnce sourceDirectoryAbsolutePath

    -- Get all the files in the project directory, recursively
    absoluteIgnorePaths <- canonIgnorePaths sourceDirectoryAbsolutePath ignorePaths
    filesToCopy <- getFilePathsRecursive absoluteIgnorePaths sourceDirectoryAbsolutePath
    allCopiedFiles <- handleAllFiles library sourceDirectoryAbsolutePath outputDirectoryAbsolutePath filesToCopy
    -- Build the phlog from the collected file paths and frontmatter.
    let phlogMeta = filterToPhlogMeta allCopiedFiles
    -- FIXME: doing the maybe logic here and elsewhere is very bad--should occur in Config.hs with defaults\
    buildPhlogIndexes
        (library.config.server.hostname)
        (fromMaybe 70 (fromIntegral <$> library.config.server.listenPort))
        outputDirectoryAbsolutePath
        phlogMeta