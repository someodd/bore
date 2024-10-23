{- | Text file search and rank functionality.

Designed for use with the Bore SpacecookieClone, for the Gopher Protocol, in order to
implement search.

The terms "rank" and "score" are used interchangeably, but the `RankScore` is implemented
as a float where higher is better. Various factors which go into the score can be
manipuluated through the weights represented as constants in this module.

Possible improvements include using FrontMatter in ranking.

-}
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

-- | The relevancy score of a document as it pertains to some set of keywords.
type RankScore = Float

-- | Define the size of the context window (number of words before and after)
contextWindowSize :: Int
contextWindowSize = 10

-- | Minimum score to be included in the search results.
scoreThreshold :: Float
scoreThreshold = 1

-- | Weight constant for proximity ordered bonus.
weightProximityOrdered :: Float
weightProximityOrdered = 30

-- | Weight constant for fuzzy match score.
weightFuzzyMatch :: Float
weightFuzzyMatch = 1.0

-- | Weight constant for keyword frequency score.
weightFrequency :: Float
weightFrequency = 2.0

-- | Weight for each exact match.
weightExactMatch :: Float
weightExactMatch = 2.0

-- | Weight for keywords appearing in selector (path).
weightSelector :: Float
weightSelector = 50.0

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
    :: FilePath
    -- ^ The selector belonging to the document.
    -> [Text]
    -- ^ Keywords to search for.
    -> Text
    -- ^ Document to rank/score.
    -> (RankScore, [ContextSnippet])
    -- ^ The score along with the "contexts" where matches occur.
rankDocument selector keywords content =
  let contentWords = T.words . toLower $ cleanText content
      (totalScore, keywordMatches) = computeTotalScore selector keywords contentWords
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


-- | Compute total score based on proximity of different keyword matches and fuzzy matches
--
-- The total score is a combination of the following:
--
-- * Proximity score: The closer the keywords are to each other, the higher the score. A bonus for the same order, too.
-- * Frequency score: The more a keyword appears in the content, the higher the score.
-- * Fuzzy match score: The closer the fuzzy match is to the keyword, the higher the score.
-- * Exact match score: The more exact matches, the higher the score.
--
-- Example:
-- >>> tokens = T.words "Do you like tags? I like tags."
-- >>> keywords = ["I", "like", "tags"]
-- >>> fst $ computeTotalScore keywords tokens
-- 300.0
computeTotalScore :: FilePath -> [Text] -> [Text] -> (RankScore, [(Text, Int)])
computeTotalScore selector keywords contentWords =
  let
    (keywordMatches, exactMatchScore) = computeExactMatchScore keywords contentWords
    finalScore = sum
      [ computeSelectorScore keywords selector
      , keywordProximity keywordMatches
      , computeFrequencyScore keywords contentWords
      , computeFuzzyMatchScore keywords contentWords
      , exactMatchScore
      ]
  in
    (finalScore, keywordMatches)

-- | The score for keywords appearing in the selector (path).
computeSelectorScore :: [Text] -> FilePath -> RankScore
computeSelectorScore keywords selector =
  let
    selectorWords = splitWords . toLower $ cleanText (T.pack selector)
    selectorMatches = findKeywordMatches keywords selectorWords
    selectorScore = fromIntegral $ 10 * length selectorMatches
  in
    selectorScore * weightSelector

computeExactMatchScore :: [Text] -> [Text] -> ([(Text, Int)], RankScore)
computeExactMatchScore keywords contentWords =
  let
    keywordMatches = findKeywordMatches keywords contentWords
    preliminaryScore = fromIntegral $ 10 * length keywordMatches
  in
    (keywordMatches, preliminaryScore * weightExactMatch)

computeFrequencyScore :: [Text] -> [Text] -> RankScore
computeFrequencyScore keywords contentWords =
  let
    preliminaryScore = fromIntegral $ 5 * sum (map (keywordFrequency contentWords) keywords)
  in
    preliminaryScore * weightFrequency

computeFuzzyMatchScore :: [Text] -> [Text] -> RankScore
computeFuzzyMatchScore keywords contentWords =
  let
    preliminaryScore = sum $ map (\kw -> if kw `elem` contentWords then 100 else fst (fuzzyMatch kw contentWords)) keywords
  in
    weightFuzzyMatch * preliminaryScore

-- | Calculate proximity score for different keywords based on their positions. The closer
-- the keywords are to each other, the higher the score. A significant bonus is applied for
-- keywords appearing in the same order with small gaps.
keywordProximity :: [(Text, Int)] -> RankScore
keywordProximity keywordMatches =
  let positions = map snd keywordMatches
      -- Calculate proximity between different keywords
      keywordPairs = zip positions (tail positions)  -- Consecutive positions for order bonus
      distances = map (\(i, j) -> abs (i - j)) keywordPairs
      -- Apply bonus if keywords are in the same order and close proximity note this also
      -- gives the same bonus for cases like searching for "hello world" and "hello hello
      -- hello world"
      orderBonus = (if all (uncurry (<)) keywordPairs then 30 else 0) * weightProximityOrdered
  in if null distances
     then 0
     else max 0 (50 - fromIntegral (minimum distances)) + orderBonus -- Closer proximity scores higher, add order bonus

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
    -> (RankScore, Int)
    -- ^ The score and the best match word index.
fuzzyMatch keyword contentWords =
  let distances = zipWith (\i word -> (editDistance keyword word, i)) [0 ..] contentWords
      (minDistance, bestMatchIndex) = minimumBy (comparing fst) distances
      score = max 0 (100 - minDistance) -- Fuzzy score decreases with larger distance
   in (fromIntegral score, bestMatchIndex)

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
searchDocuments :: [Text] -> AbsolutePath -> AbsolutePath -> IO [(FilePath, Bool, RankScore, [Text])]
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
          let relativePath = makeRelative absoluteOutputPath fp
              (score, contexts) = rankDocument relativePath keywords content
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
searchResponse :: AbsolutePath -> Text -> [(FilePath, Bool, RankScore, [Text])] -> GopherResponse
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
makeSearchResult :: AbsolutePath -> (FilePath, Bool, RankScore, [Text]) -> [GopherMenuItem]
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
