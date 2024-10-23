-- FIXME: the duplication of having to figure out the tag paths over and over is bad and
-- leads to errors.
{- | Construct phlog indexes.

This module works by having an entrypoint which you can pass filemeta to and
tag indexes and the general main phlog index is constructed from the frontmatter
associated with the filemeta passed.

-}
module Bore.Phlog (filterToPhlogMeta, buildPhlogIndexes) where

import Bore.FrontMatter
import Bore.FileLayout
import Bore.Text.Gophermap

import Data.List (nub, sortOn, isPrefixOf)
import Time.Types (dateYear)
import Data.Dates.Parsing
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Data.Ord (Down(..))

{- | Determines if the frontmatter indicates a phlog post.

Simply checks if the base directory of the file is the phlog directory.

This won't work for symlinks (?) and doesn't guard against `../`.

-}
isPhlogPost :: RelativePath -> Bool
isPhlogPost relativePathToPost =
    phlogDirectory `isPrefixOf` relativePathToPost

-- FIXME: maybe delete
{- | Create a summary of a phlog post for the phlog index file

Creates the index entry for the phlog post.

Example:

-}
toPhlogSummary :: FilePath -> FrontMatter -> Text.Text
toPhlogSummary relativePath frontMatter =
    let
        title = fromMaybe "Untitled" frontMatter.title
        date = fromMaybe "No date" frontMatter.date
        tags = Text.intercalate ", " (fromMaybe ["No tags"] (frontMatter.tags))
    in
        title <> " - " <> date <> " - " <> tags <> " - " <> Text.pack relativePath

{- | Fuzzy-read the date from text.

This function attempts to interpret a date from a given text input. It uses the
`extractDateTimesY` function to extract potential date-time values, on failure
returning `Nothing`.

Examples:

>>> interpretDate "2023-10-05"
Just 2023-10-05 00:00:00 UTC
>>> interpretDate "invalid date"
Nothing
-}
interpretDate :: Text.Text -> Maybe DateTime
interpretDate text =
    case extractDateTimesY 1970 (Text.unpack text) of
        firstEntry:_ -> Just firstEntry
        _ -> Nothing

defaultNullDate :: Text.Text
defaultNullDate = "unknown"

-- | Sort phlog posts by date in descending (older as you go down/newest first) order.
sortPhlogMetaByDate :: [PhlogMeta] -> [PhlogMeta]
sortPhlogMetaByDate =
    sortOn (Down . (\(_, _, frontMatter) -> interpretDate (fromMaybe defaultNullDate (frontMatter.date))))


{- | Extract the year from the frontmatter's date.

Basically an accessor function helping us get a default year in the event a parseable
date was not found.

-}
phlogYear :: FrontMatter -> Int
phlogYear frontMatter =
    case interpretDate =<< frontMatter.date of
        Just dateTime -> dateTime.dtDate.dateYear
        Nothing -> 1970 -- Default year if date is not present or cannot be parsed

{- | Lump sorted phlog posts by year.

Produces a list of tuples where the first element is the year and the second element is a
list of `PhlogMeta` from that year.
-}
lumpPhlogMetaByYear :: [PhlogMeta] -> [(Int, [PhlogMeta])]
lumpPhlogMetaByYear sortedPhlogMeta =
    let
        years = nub $ map (\(_, _, frontMatter) -> phlogYear frontMatter) sortedPhlogMeta
    in
        map (\year -> (year, filter (\(_, _, frontMatter) -> phlogYear frontMatter == year) sortedPhlogMeta)) years

-- FIXME: could be a little bit more strict with itemtype, but that's ok.
{- | Little helper function for making a gopher menu link.

Example:

0Jargon 4.2.0   /Reference/Jargon 4.2.0 gopher.quux.org 70

-}
gopherLink :: Text.Text -> Text.Text -> Text.Text -> Text.Text -> Int -> Text.Text
gopherLink itemType label selector hostname port = (Text.intercalate "\t" [itemType <> label, selector, hostname, Text.pack . show $ port]) <> "\n"

{- | Create a label for a gopher entry based off `PhlogMeta`.
type PhlogMeta = (AbsolutePath, RelativePath, FrontMatter)
-}
toPhlogLabel :: PhlogMeta -> Text.Text
toPhlogLabel (_, relativePath, frontMatter) =
    let
        title = fromMaybe "Untitled" frontMatter.title
        date = fromMaybe "No date" frontMatter.date
        tags = Text.intercalate ", " (fromMaybe ["No tags"] (frontMatter.tags))
    in
        date <> " - " <> title <> " - " <> tags <> " - " <> Text.pack relativePath

{- | Create a gopher link to a phlog post.

-}
gopherPhlogLink :: PhlogMeta -> Text.Text -> Int -> Text.Text
gopherPhlogLink phlogMeta@(_, relativePath, frontMatter) hostname port =
    gopherLink
        (if isGophermap frontMatter then "1" else "0")
        (toPhlogLabel phlogMeta)
        ("/" <> Text.pack relativePath)
        hostname
        port

{- | Create a simple index of phlog posts.

Can be used to create different indexes if you simply filter phlogmeta by tag for example.

Creates a valid gophermap format menu.

-}
buildPhlogIndex :: Text.Text -> Int -> FilePath -> FilePath -> Text.Text -> Text.Text -> [PhlogMeta] -> IO ()
buildPhlogIndex hostname port outputDirectory outputFilename indexTitle epilogue sortedPhlogMeta = do
    -- Now create a `Text` where it lists the phlog post summaries, grouped under year headings.
    let
        lumpedByYear = lumpPhlogMetaByYear sortedPhlogMeta
        phlogIndexContent = indexTitle <> "\n\n" <> foldMap (\(year, phlogPosts) -> (Text.pack . show $ year) <> "\n" <> foldMap (\phlogMeta -> gopherPhlogLink phlogMeta hostname port) phlogPosts) lumpedByYear <> epilogue
        outputFullPath = outputDirectory </> outputFilename
    TextIO.writeFile outputFullPath (toGophermap hostname port phlogIndexContent)

{- | Create phlog indexes for each tag in the phlog meta.

-}
buildPhlogTagIndexes :: Text.Text -> Int -> FilePath -> Text.Text -> [PhlogMeta] -> IO ()
buildPhlogTagIndexes hostname port outputDirectory epilogue phlogMeta = do
    let tags = nub $ concatMap (\(_, _, frontMatter) -> fromMaybe [] (frontMatter.tags)) phlogMeta
    -- now for each tag create an index using buildPhlogIndex, passing every phlot post with that tag
    _ <- mapM (\tag -> buildPhlogIndex hostname port outputDirectory (Text.unpack tag) tag epilogue (filterPhlogMetaByTag tag phlogMeta)) tags
    pure ()

{- | Get all the tag index paths.

This could break if the buildPhlogTagIndexes write paths are actually different than seen here.
    
-}
unreliablyGetTagIndexPaths :: [PhlogMeta] -> [(Text.Text, RelativePath)]
unreliablyGetTagIndexPaths phlogMeta =
    let tags = nub $ concatMap (\(_, _, frontMatter) -> fromMaybe [] (frontMatter.tags)) phlogMeta
    in map (\tag -> (tag, phlogDirectory </> tagIndexesDirectory </> Text.unpack tag)) tags

{- | Filter some `[PhlogMeta]` by tag.

-}
filterPhlogMetaByTag :: Text.Text -> [PhlogMeta] -> [PhlogMeta]
filterPhlogMetaByTag tag phlogMeta = filter (\(_, _, frontMatter) -> tag `elem` fromMaybe [] (frontMatter.tags)) phlogMeta

-- | (Absolute path, relative path, frontmatter)
type PhlogMeta = (AbsolutePath, RelativePath, FrontMatter)

{- | Filter out all file meta which lack minimal phlog frontmatter.

-}
filterToPhlogMeta :: [(FilePath, FilePath, Maybe FrontMatter)] -> [PhlogMeta]
filterToPhlogMeta copiedFileMeta = do
    [(fullPath, relativePath, frontMatter) | (fullPath, relativePath, Just frontMatter) <- copiedFileMeta, isPhlogPost relativePath]

-- TODO: could add a manual "summary" field to the frontmatter to use in the index?
{- | Create an atom feed of phlog posts.

Assumes the phlog meta passed is already sorted by date.

This could be very slightly tweaked so you could give it a label and just filter what you
pass as sortedPhlogMeta. For example, generating feeds for each tag.

Currently has the issue of not handling actual URI paths because there's currently no way
to set the base URI for the feed or in general for gopher with this app. I think most feed
readers will correctly interpret paths as relative to the host (port, domain).

-}
buildPhlogFeed :: FilePath -> Text.Text -> [PhlogMeta] -> IO FilePath
buildPhlogFeed writePath feedTitle sortedPhlogMeta = do
    let
        removeGophermapFileName path =
            if takeFileName path == gophermapFileName
            then takeDirectory path
            else path
        feedEntries = map (\(_, relativePath, frontMatter) ->
            let
                title = fromMaybe "Untitled" (frontMatter.title)
                date = fromMaybe "No date" (frontMatter.date)
                summary = toPhlogSummary relativePath frontMatter
                cleanedRelativePath = removeGophermapFileName relativePath
            in
                "<entry>\n" <>
                "<title>" <> title <> "</title>\n" <>
                "<link href=\"/" <> Text.pack cleanedRelativePath <> "\" />\n" <>
                "<updated>" <> date <> "</updated>\n" <>
                "<summary>" <> summary <> "</summary>\n" <>
                "</entry>\n"
            ) sortedPhlogMeta

        feedContent =
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" <>
            "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n" <>
            "<title>" <> feedTitle <> "</title>\n" <>
                        "<updated>" <> fromMaybe "No date" (listToMaybe sortedPhlogMeta >>= \(_, _, frontMatter) -> frontMatter.date) <> "</updated>\n" <>
                        mconcat feedEntries <>
                        "</feed>\n"

    TextIO.writeFile writePath feedContent
    pure writePath

{- | Create Text which links to all the phlog indexes.

-}
phlogIndexLinks :: Text.Text -> Int -> [(Text.Text, RelativePath)] -> RelativePath -> RelativePath -> Text.Text
phlogIndexLinks hostname port tagLinks mainIndexLink feedLink =
    let
        content =
            "\n\nPHLOG INDEXES\n\n" <>
            gopherLink "1" "Main Phlog Index" (Text.pack mainIndexLink) hostname port <>
            gopherLink "0" "Phlog Atom Feed" (Text.pack feedLink) hostname port <>
            "\nTags\n" <>
            foldMap (\(tag, tagLink) -> gopherLink "1" tag (Text.pack $ "/" </> tagLink ) hostname port) tagLinks
    in
        toGophermap hostname port content

{- | Build the phlog from a list of phlog files' frontmatter and their respective file paths.

(Absolute path, relative path, frontmatter)

@@hostname@@: used to construct the gophermap links.
@@port@@: used to construct the gophermap links.

-}
buildPhlogIndexes :: Text.Text -> Int -> FilePath -> [PhlogMeta] -> IO ()
buildPhlogIndexes hostname port outputPath phlogMeta = do
    let
        sortedPhlogMeta = sortPhlogMetaByDate phlogMeta
        unreliableTagPaths = unreliablyGetTagIndexPaths sortedPhlogMeta
        mainPhlogIndexPath = "/" </> phlogDirectory
        phlogFeedPath = "/" </> phlogDirectory </> phlogFeedName
        epilogue = phlogIndexLinks hostname port unreliableTagPaths mainPhlogIndexPath phlogFeedPath

    _ <- createDirectoryIfMissing True (outputPath </> phlogDirectory </> tagIndexesDirectory)
    _ <- createDirectoryIfMissing True (outputPath </> phlogDirectory)
    _ <- buildPhlogIndex hostname port (outputPath </> phlogDirectory) gophermapFileName "All Phlog Posts" epilogue sortedPhlogMeta
    _ <- buildPhlogTagIndexes hostname port (outputPath </> phlogDirectory </> tagIndexesDirectory) epilogue sortedPhlogMeta
    _ <- buildPhlogFeed (outputPath </> phlogDirectory </> phlogFeedName) "Main Phlog Index" sortedPhlogMeta
    pure ()