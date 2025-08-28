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
module Bore.Parse (buildTree, applyDevMode) where

import Bore.FrontMatter
import Bore.Text.Template
import Bore.Library
import Bore.Phlog
import Bore.FileLayout
import Bore.Text.Gophermap
import Bore.Config

import qualified Data.Text.IO as TextIO
import System.Directory (canonicalizePath)
import System.FilePath (splitPath)
import System.FilePath (makeRelative)
import Data.Maybe (fromMaybe, catMaybes)
import System.Directory (doesDirectoryExist)
import qualified Data.Text as Text

-- FIXME: use constant
isInPhlog :: FilePath -> Bool
isInPhlog path = take 1 (splitPath path) == ["phlog/"]

-- | Manipulate frontmatter for possible phlog posts.
--
-- * If the supplied frontmatter has a parent defined, do nothing.
-- * If the supplied `FrontMatter` does not have a parent defined, and the file is in the
--   phlog directory, set the parent to "post.txt".
prepPostFrontMatter
    :: FilePath
    -- ^ The RELATIVE path to the file.
    -> FrontMatter
    -- ^ The frontmatter of the file.
    -> FrontMatter
    -- ^ The frontmatter with possible modifications.library
prepPostFrontMatter relativePath' fm = do
    if null (parent fm) && isInPhlog relativePath'
        then fm { parent = Just . Text.pack $ templatePostFileName }
        else fm

-- FIXME: why is fileContents vs fileContentsStripped happening? why is it defaulting to ""
-- | Run templating (Mustache) on the file contents passed, according to the frontmatter
-- and possibly other factors.
--
-- IO because templating may use IO. Ideally, all of the templates would be loaded into
-- the template cache once and we'd be done with IO.
possiblyTemplate :: Maybe FrontMatter -> Text.Text -> Maybe Text.Text -> Library -> AbsolutePath -> IO Text.Text
possiblyTemplate frontMatterMaybe fileContents fileContentsStripped library projectDirectory = case frontMatterMaybe of
    Just frontMatter -> do
        --parseTemplate library templateDir maybeFrontMatter currentText
        --templateCache <- loadAllTemplatesIntoCache (projectDirectory </> templateDirectory)  -- FIXME: needs to go to library!!!
        if fromMaybe False frontMatter.skipTemplating
            then pure fileContents
            else do
                --parseTemplate library projectDirectory templateCache (Just frontMatter) (fromMaybe "" fileContentsStripped)
                --parseTemplate library projectDirectory (Just frontMatter) (fromMaybe "" fileContentsStripped)
                renderTemplate library projectDirectory (Just frontMatter) (fromMaybe "" fileContentsStripped)
                -- library projectDirectory (Just frontMatter) (fromMaybe "" fileContentsStripped)
                --renderTemplate projectDirectory (fromMaybe "" fileContentsStripped) (Text.unpack <$> frontMatter.parent) []
    Nothing -> pure fileContents

-- | Possibly transform the text into a gophermap according to its frontmatter (or possibly other factors?).
possiblyGopherize :: Text.Text -> Maybe FrontMatter -> Library -> Text.Text
possiblyGopherize someText maybeFrontMatter library = do
    case maybeFrontMatter of
        Just frontMatter -> do
            if isGophermap frontMatter
                then
                    let
                        hostname = library.config.server.hostname
                        port = fromMaybe 70 (fromIntegral <$> library.config.server.hostPort)
                    in
                        toGophermap hostname port someText
                else
                    someText
        Nothing -> someText

{- | Parse a file, returning the full destination path, the relative destination path, and
(maybe) the frontmatter.

sourceFile is the absolute path to the file being parsed.

-}
parseFile :: Library -> FilePath -> FilePath -> FilePath -> IO (Maybe (FilePath, RelativePath, Maybe FrontMatter))
parseFile library projectDirectory sourceFile destination = do
    fileContents <- TextIO.readFile sourceFile
    -- FIXME: relative gets grabbed twice.
    let relativePath' = makeRelative projectDirectory sourceFile

    -- Now do a lot of prep/operations on this file's frontmatter, which may or may not exist.
    let
        frontMatterAndRestMaybe = parseFrontMatterIgnoreErrors fileContents
        frontMatterMaybe = prepPostFrontMatter relativePath' <$> (fst <$> frontMatterAndRestMaybe)
        fileContentsStripped = snd <$> frontMatterAndRestMaybe

    -- Parse with Mustache if necessary.
    afterTemplatingText <- possiblyTemplate frontMatterMaybe fileContents fileContentsStripped library projectDirectory
    
    -- Probably important that turning it into a gopher menu is the last step.
    let possiblyGopherizedText = possiblyGopherize afterTemplatingText frontMatterMaybe library

    -- If the frontmatter's draft setting is true, we don't want to write the file out.
    case frontMatterMaybe >>= (.draft) of
        Just True -> do
            putStrLn $ "Skipping file because it's a draft: " ++ sourceFile
            pure Nothing
        _ -> do
            -- We're done transforming the damn thing! Write the file out and return useful data.
            (relativePath, fullDestination) <- writeDest projectDirectory sourceFile destination possiblyGopherizedText
            pure $ Just (fullDestination, relativePath, frontMatterMaybe)


{- | Function which handles a file in the walking process.

Focused on side-effects (writing).

If it's a directory, the return is nothing.

Changes the output destination if the file is a `parseableGophermapFileName`.

@@outputDirectory@@ is the full absolute path to the output/destination directory.
@@filePath@@ absolute path to the file being handled. Should be absolute path to the source file.

-}
handleFile :: Library -> FilePath -> FilePath -> FilePath -> IO (Maybe (FilePath, FilePath, Maybe FrontMatter))
handleFile library sourceDirectory outputDirectory filePath = do
    -- It's important to check if it's a directory first, because we don't want to match
    -- directories as parseable files just because they have a file extension we are
    -- looking for.
    isDir <- doesDirectoryExist filePath
    if isDir
        then do
            putStrLn $ "creating directory: " ++ filePath
            _ <- createDestinationDirectoryForFile outputDirectory sourceDirectory filePath
            pure Nothing
        else if takeFullExtension filePath `elem` onlyParse
            then do
                putStrLn $ "parse the file: " ++ filePath
                parseFile library sourceDirectory filePath outputDirectory
            else do
                putStrLn $ "only copy file: " ++ filePath
                -- FIXME: this is copying files and making a directory for them that includes their name!
                (fullPath, relativePath) <- onlyCopyFile sourceDirectory outputDirectory filePath
                pure $ Just (fullPath, relativePath, Nothing)

{- | Helper function to get all the handle all files passed, returning metadata regarding
the files which were actually copied.

Also handles the creation of directories.

-}
handleAllFiles :: Library -> FilePath -> FilePath -> [FilePath] -> IO [(FilePath, FilePath, Maybe FrontMatter)]
handleAllFiles library projectDirectoryAbsolutePath outputDirectoryAbsolutePath filesToCopy =
    catMaybes <$> mapM (handleFile library projectDirectoryAbsolutePath outputDirectoryAbsolutePath) filesToCopy


-- | Apply development mode settings to a config if needed
--
-- This is especially relevant to parsing because of the "fill in the blanks" nature of
-- creating gophermaps, where the host/port is assumed.
applyDevMode :: Config -> Bool -> Config
applyDevMode config True = 
  config { server = (server config) { hostname = Text.pack "localhost" } }
applyDevMode config False = config

{- | Main entrypoint to build the entire gopherhole project.

The file paths can be absolute or relative, they will be made absolute.

-}
buildTree :: FilePath -> FilePath -> Bool-> IO ()
buildTree sourceDirectory outputDirectory devMode = do
    sourceDirectoryAbsolutePath <- canonicalizePath sourceDirectory
    outputDirectoryAbsolutePath <- canonicalizePath outputDirectory

    _ <- resetOutputDirectory outputDirectoryAbsolutePath

    libraryBeforeCheck <- loadOnce sourceDirectoryAbsolutePath
    -- apply applyDevMode to the config in library
    let library = libraryBeforeCheck { config = applyDevMode (config libraryBeforeCheck) devMode }

    -- Get all the files in the project directory, recursively
    absoluteIgnorePaths <- canonIgnorePaths sourceDirectoryAbsolutePath ignorePaths
    filesToCopy <- getFilePathsRecursive absoluteIgnorePaths sourceDirectoryAbsolutePath
    allCopiedFiles <- handleAllFiles library sourceDirectoryAbsolutePath outputDirectoryAbsolutePath filesToCopy
    -- Build the phlog from the collected file paths and frontmatter.
    let phlogMeta = filterToPhlogMeta allCopiedFiles
    -- FIXME: doing the maybe logic here and elsewhere is very bad--should occur in Config.hs with defaults\
    buildPhlogIndexes
        (library.config.server.hostname)
        (fromMaybe 70 (fromIntegral <$> library.config.server.hostPort))
        outputDirectoryAbsolutePath
        phlogMeta