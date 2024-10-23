-- TODO: BETTER DOCUMENT HOW SCORING WORKS AND EXPLAIN SCORES
{-# LANGUAGE LambdaCase #-}

module Bore.SpacecookieClone.Search (getSearchResults) where

import Bore.Text.Clean
import Bore.FileLayout
import Bore.FrontMatter
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.List (findIndices, foldl', minimumBy)
import Data.List qualified as L
import Data.Ord (comparing)
import Data.Text (Text, toLower, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO
import Network.Gopher
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>), makeRelative)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

-- | Define the size of the context window (number of words before and after)
contextWindowSize :: Int
contextWindowSize = 10

-- | Minimum score to be included in the search results.
scoreThreshold :: Int
scoreThreshold = 1

-- | Data structure to hold context snippet along with its start and end indices
data ContextSnippet = ContextSnippet
  { csStart :: Int,
    csEnd :: Int,
    csText :: Text
  }
  deriving (Show)

-- | Score the text for how well it matches keywords and return the score along with
-- snippets of where the matches were found.
rankDocument
    :: [Text]
    -- ^ Keywords to search for.
    -> Text
    -- ^ Document to rank/score.
    -> (Int, [ContextSnippet])
    -- ^ The score along with the "contexts" where matches occur.
rankDocument keywords content =
  let contentWords = T.words . toLower $ cleanText content
      keywordMatches = findKeywordMatches keywords contentWords
      totalScore = computeTotalScore keywords keywordMatches contentWords
      contexts = extractKeywordContexts keywordMatches contentWords
  in (totalScore, contexts)

-- TODO: what about fuzzy matches? Otherwise won't get contexts for fuzzy match.
-- | Find all keyword *exact* matches in the document and their positions
-- | Find all keyword matches (both exact and fuzzy) in the document and their positions
findKeywordMatches :: [Text] -> [Text] -> [(Text, Int)]
findKeywordMatches keywords contentWords = concatMap findMatchesForKeyword keywords
  where
    findMatchesForKeyword :: Text -> [(Text, Int)]
    findMatchesForKeyword keyword =
      let exactMatches = [(keyword, idx) | idx <- findIndices (== keyword) contentWords]
          fuzzyMatches = [(keyword, idx) | (_, idx) <- map (fuzzyMatch keyword) [contentWords]]
      in exactMatches ++ fuzzyMatches


-- | Compute total score based on proximity of different keyword matches
-- | Compute total score based on proximity of different keyword matches and fuzzy matches
computeTotalScore :: [Text] -> [(Text, Int)] -> [Text] -> Int
computeTotalScore _ keywordMatches contentWords =
  let proximityScore = keywordProximity keywordMatches
      frequencyScore = 5 * sum (map (keywordFrequency contentWords . fst) keywordMatches)
      fuzzyMatchScore = sum $ map (\(kw, _) -> if kw `elem` contentWords then 100 else fst (fuzzyMatch kw contentWords)) keywordMatches
  in fuzzyMatchScore + proximityScore + frequencyScore

-- | Calculate proximity score for different keywords based on their positions
keywordProximity :: [(Text, Int)] -> Int
keywordProximity keywordMatches =
  let positions = map snd keywordMatches
      -- Calculate proximity between different keywords
      keywordPairs = [(i, j) | i <- positions, j <- positions, i < j]
      distances = map (\(i, j) -> abs (i - j)) keywordPairs
  in if null distances then 0 else max 0 (50 - minimum distances) -- Closer proximity scores higher

-- | Count the number of times a keyword appears in the content
keywordFrequency :: [Text] -> Text -> Int
keywordFrequency contentWords keyword =
  length $ filter (== keyword) contentWords

-- | Calculate fuzzy match score and get index of best match
--
-- Highest potential score is 100. Pointss deducted for difference between the best match
-- and the keyword.
--
-- Example:
-- >>> fuzzyMatch "togs" ["do", "you", "like", "tags?"]
-- (98,3)
fuzzyMatch
    :: Text
    -- ^ keyword to search for.
    -> [Text]
    -- ^ The words (tokens) to search in.
    -> (Int, Int)
    -- ^ The score and the best match word index.
fuzzyMatch keyword contentWords =
  let distances = zipWith (\i word -> (editDistance keyword word, i)) [0 ..] contentWords
      (minDistance, bestMatchIndex) = minimumBy (comparing fst) distances
      score = max 0 (100 - minDistance) -- Fuzzy score decreases with larger distance
   in (score, bestMatchIndex)

-- | Helper function to calculate Levenshtein distance using 'Text.EditDistance'
editDistance :: Text -> Text -> Int
editDistance keyword word =
  levenshteinDistance defaultEditCosts (unpack keyword) (unpack word)

-- | Extract and merge contexts for the found keyword matches, highlighting all keywords
extractKeywordContexts :: [(Text, Int)] -> [Text] -> [ContextSnippet]
extractKeywordContexts keywordMatches contentWords =
  let sortedMatches = L.sortOn snd keywordMatches  -- Sort matches by index
      mergedContexts = mergeCloseMatches sortedMatches
      contextSnippets = map (\(startIdx, endIdx) -> extractContextWithHighlights keywordMatches contentWords startIdx endIdx) mergedContexts
  in contextSnippets

-- | Merge keyword matches that are close enough to form a single context window
-- | Merge keyword matches that are close enough to form a single context window, but extend the context window before and after the match.
mergeCloseMatches :: [(Text, Int)] -> [(Int, Int)]
mergeCloseMatches [] = []
mergeCloseMatches ((_, idx):xs) = foldl' merge [(idx - contextWindowSize, idx + contextWindowSize)] xs
  where
    merge [] (_, nextIdx) = [(nextIdx - contextWindowSize, nextIdx + contextWindowSize)]  -- If acc is empty, start a new context
    merge acc@((start, end):rest) (_, nextIdx)
      | nextIdx - end <= contextWindowSize = (start, max (nextIdx + contextWindowSize) end) : rest -- Extend the current context
      | otherwise = (nextIdx - contextWindowSize, nextIdx + contextWindowSize) : acc  -- Start a new context

-- | Extract a context snippet and highlight all keywords within the context
-- | Extract a context snippet and highlight all keywords within the extended context window.
extractContextWithHighlights :: [(Text, Int)] -> [Text] -> Int -> Int -> ContextSnippet
extractContextWithHighlights keywordMatches contentWords startIdx endIdx =
  let contextWords = take (endIdx - startIdx + 1) $ drop startIdx contentWords
      highlightedWords = map (\(i, word) -> if any (\(_, kwIdx) -> kwIdx == i) keywordMatches then "[" <> word <> "]" else word) (zip [startIdx..] contextWords)
  in ContextSnippet startIdx endIdx (T.unwords highlightedWords)


-- | Check if the new snippet overlaps with existing ones based on indices
contextsOverlap :: ContextSnippet -> ContextSnippet -> Bool
contextsOverlap cs1 cs2 =
  csStart cs1 <= csEnd cs2 && csEnd cs1 >= csStart cs2

-- | Combine contexts, ensuring no overlaps
combineContexts :: [ContextSnippet] -> [ContextSnippet]
combineContexts = foldl' addContext []

addContext :: [ContextSnippet] -> ContextSnippet -> [ContextSnippet]
addContext acc cs =
  if any (contextsOverlap cs) acc
    then acc
    else acc ++ [cs]

-- | Function to process multiple documents
searchDocuments :: [Text] -> AbsolutePath -> AbsolutePath -> IO [(FilePath, Bool, Int, [Text])]
searchDocuments keywords sourceDirectoryAbsolutePath absoluteOutputPath = do
  docPaths <- getTxtFiles absoluteOutputPath
  docs <-
    mapM
      ( \fp -> do
          content <- loadFileContent fp
          (isMenu, hackedContent) <- gophermapHack fp content sourceDirectoryAbsolutePath absoluteOutputPath
          pure (fp, isMenu, hackedContent)
      )
      docPaths
  return $
    map
      ( \(fp, isMenu, content) ->
          let (score, contexts) = rankDocument keywords content
              nonOverlappingContexts = combineContexts contexts
              contextTexts = map csText nonOverlappingContexts
           in (fp, isMenu, score, contextTexts)
      )
      docs

-- | Check if a file is a gophermap. If it is, return the content without the gophermap syntax.
gophermapHack :: FilePath -> Text -> AbsolutePath -> AbsolutePath -> IO (Bool, Text)
gophermapHack fp content sourceDirectoryAbsolutePath absoluteOutputPath = do
  let
    filePathRelative = makeRelative absoluteOutputPath fp
    pathHackToLoadFrontMatter = sourceDirectoryAbsolutePath </> filePathRelative
  frontMatterContent <- loadFileContent pathHackToLoadFrontMatter
  isGophermapFlag <- isGophermapFile frontMatterContent
  if isGophermapFlag
    then pure (True, removeGophermapSyntax content)
    else pure (False, content)

-- Recursively search for .txt files in a directory
getTxtFiles :: FilePath -> IO [FilePath]
getTxtFiles dir = do
  contents <- listDirectory dir
  paths <-
    mapM
      ( \name -> do
          let path = dir </> name
          isDir <- doesDirectoryExist path
          if isDir then getTxtFiles path else return [path]
      )
      contents
  return $ filter (\f -> takeExtension f == ".txt") (concat paths)

-- Load the content of a file
loadFileContent :: FilePath -> IO Text
loadFileContent = TIO.readFile

-- FIXME: no need IO
-- Parse the frontmatter of a file and check if it's a gophermap
isGophermapFile :: Text -> IO Bool
isGophermapFile content =
  case parseFrontMatter content of
    Right (frontmatter, _) -> do
      pure $ isGophermap frontmatter
    Left _ -> do
      pure False

-- | Remove the gophermap syntax from a file.
--
-- This helps ensure that gopher protocol code doesn't appear in search results when all we care about is the labels of menu items.
--
-- We process lines that have tabs and extract the label before the tab.
removeGophermapSyntax :: Text -> Text
removeGophermapSyntax =
  T.unlines
    . map
      ( \line ->
          let (label, _) = T.breakOn "\t" line
           in T.drop 1 label
      )
    . T.lines

-- Main entrypoint. Should also have a prefix about search results when blank...
getSearchResults :: Text -> AbsolutePath -> AbsolutePath -> IO GopherResponse
getSearchResults query sourceDirectoryAbsolutePath absoluteOutputPath = do
  documentResults <- searchDocuments (T.words query) sourceDirectoryAbsolutePath absoluteOutputPath
  let prunedResults = filter (\(_, _, s, _) -> s >= scoreThreshold) documentResults
  pure $ searchResponse absoluteOutputPath query $ L.sortOn (\(_, _, s, _) -> negate s) prunedResults

makeInfoLine :: B.ByteString -> GopherMenuItem
makeInfoLine t = Item InfoLine t "" Nothing Nothing

-- Function to generate a GopherResponse for search results
searchResponse :: AbsolutePath -> Text -> [(FilePath, Bool, Int, [Text])] -> GopherResponse
searchResponse absoluteOutputPath query files =
  let actualResults = concatMap (makeSearchResult absoluteOutputPath) files
      preamble =
        Item
          InfoLine
          (if null files then "No decent search results found for \"" <> E.encodeUtf8 query <> "\"" else ("Search results for \"" <> E.encodeUtf8 query <> "\""))
          ""
          Nothing
          Nothing
   in MenuResponse $ preamble : actualResults

-- Build a search result for a file
makeSearchResult :: AbsolutePath -> (FilePath, Bool, Int, [Text]) -> [GopherMenuItem]
makeSearchResult absoluteOutputPath (fp, isMenu, _, contexts) =
  makeLink selector : map makeSummary contexts
  where
    selector = "/" </> makeRelative absoluteOutputPath fp
    makeSummary summary' = makeInfoLine $ E.encodeUtf8 summary'
    makeLink selector' =
      Item
        (if isMenu then Directory else File)
        (C8.pack selector')
        (C8.pack selector')
        Nothing
        Nothing
