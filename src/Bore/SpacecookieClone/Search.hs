-- TODO: Am I not stripping punctuation from keywords and tokens? That's a big deal. Also
-- need to ensure a standard function for doing so like `searchableText`.
-- TODO: improvements: exclude words like and, is, etc.
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

-- | The minimum length of a keyword to be considered for fuzzy matching. This is very
-- English-centric. For example, this would be foolish for Chinese, although it could use
-- pinyin, romanization of various logographic languages or whatever.
minFuzzyKeywordLength :: Int
minFuzzyKeywordLength = 4

-- | The minimum likeness percentage for a fuzzy match to be considered.
minimumFuzzyLikeness :: Float
minimumFuzzyLikeness = 75

-- | Define the size of the context window (number of words before and after)
contextWindowSize :: Int
contextWindowSize = 5

-- | Minimum score to be included in the search results.
scoreThreshold :: Float
scoreThreshold = 200

-- | Weight constant for ordered proximity bonus
weightOrderedProximity :: Float
weightOrderedProximity = 1000

-- | Weight constant for fuzzy match score.
--
-- This weighs against the average percentage of best likeness of all keywords.
weightFuzzyMatch :: Float
weightFuzzyMatch = 0.1

-- | Weight constant for keyword frequency score.
weightFrequency :: Float
weightFrequency = 2.0

-- | Weight for each exact match.
weightExactMatch :: Float
weightExactMatch = 10.0

-- | Weight for keywords exactly appearing in selector (path).
weightSelectorExact :: Float
weightSelectorExact = 100.0

-- | Weight for keywords fuzzy appearing in selector (path).
weightSelectorFuzzy :: Float
weightSelectorFuzzy = weightFuzzyMatch

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
    -> (String, RankScore, [ContextSnippet])
    -- ^ The highlighted selector, the score, and the "contexts" where matches occur (in the body).
rankDocument selector keywords content =
  let contentWords = T.words . toLower $ cleanText content
      (highlightedSelector, totalScore, keywordMatches) = computeTotalScore selector keywords contentWords
      contexts = extractKeywordContexts keywordMatches contentWords
  in (highlightedSelector, totalScore, contexts)

-- | Find all keyword *exact* matches in the document and their positions
-- | Find keyword matches (both exact and fuzzy) in the document and their positions.
--
-- Finds all exact matches, but if there are none, try to find the best fuzzy match.
--
-- Will skip fuzzy matches if the keyword is too short.
findKeywordMatches
  :: [Text]
  -> [Text]
  -> [(Text, Int, Float)]
  -- ^ The Float is the percentage likeness of the fuzzy match. For exact match it's 100.
findKeywordMatches keywords contentWords = concatMap findMatchesForKeyword keywords
  where
    findMatchesForKeyword :: Text -> [(Text, Int, Float)]
    findMatchesForKeyword keyword =
      case [(keyword, idx, 100) | idx <- findIndices (== keyword) contentWords] of
        [] ->
          if T.length keyword >= minFuzzyKeywordLength
            then
              filter (\(_, _, likeness) -> likeness >= minimumFuzzyLikeness)
                [ let (likeness, idx) = bestFuzzyMatch keyword contentWords in (keyword, idx, likeness)
                ]
            else
              []
        exactMatches -> exactMatches


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
computeTotalScore
  :: FilePath
  -> [Text]
  -> [Text]
  -> (String, RankScore, [(Text, Int)])
  -- ^ The new selector with matches highlighted, the final score, and the keyword matches (with their indexes).
computeTotalScore selector keywords contentWords =
  let
    (keywordMatches, exactMatchScore) = computeMatchScore keywords contentWords
    (selectorHighlighted, selectorScore) = computeSelectorScore keywords selector
    finalScore = sum
      [ selectorScore
      , keywordOrderedProximity keywordMatches
      , computeFrequencyScore keywords contentWords
      --, computeFuzzyMatchScore keywords contentWords
      , exactMatchScore
      ]
  in
    (selectorHighlighted, finalScore, keywordMatches)

-- | The score for keywords (fuzzy) appearing in the selector (path).
--
-- Currently has no weighted difference between exact and fuzzy matches. I think this is
-- fine since only one fuzzy match (the best) ever gets considered.
--
-- Example:
-- >>> computeSelectorScore ["hello", "world"] "test/this/hello_world-foo bar.foo"
-- 20000.0
--- >>> computeSelectorScore ["gopher"] "test/this/hello_world-foo bar.gopher.txt"
-- 16.666672
--- >>> computeSelectorScore ["gopher"] "test/this/hello_gopher-foo bar.gopher.txt"
-- 10000.0
-- >>> computeSelectorScore ["world"] "hello/world/foo.txt"
-- 10000.0
computeSelectorScore
  :: [Text]
  -> FilePath
  -> (String, RankScore)
  -- ^ The new selector with matches highlighted and the score.
computeSelectorScore keywords selector =
  let
    selectorWords' = selectorWords selector
    wordLikeness =
      [ (word, indx, if likeness == 100 then likeness * weightSelectorExact else likeness * weightSelectorFuzzy)
      | (word, indx, likeness) <- findKeywordMatches keywords selectorWords'
      ]
    likenessSum = sum $ map (\(_, _, likeness) -> likeness) wordLikeness
    -- Highlight matches directly within `selector` using start positions
    highlightedSelector = 
      foldr
        (\(word, indx, _) acc ->
            let (prefix, rest) = splitAt (T.length (T.unwords $ take indx selectorWords')) acc
                (toHighlight, suffix) = splitAt (T.length word) rest
            in prefix <> "[" <> toHighlight <> "]" <> suffix
        )
        selector
        wordLikeness
  in
    (highlightedSelector, likenessSum)

-- Change this to do the fuzzy match and just do it off the batt and give bonus for 100% match.
computeMatchScore :: [Text] -> [Text] -> ([(Text, Int)], RankScore)
computeMatchScore keywords contentWords =
  let
    keywordMatches = findKeywordMatches keywords contentWords
    keywordMatchesBonus = [if likeness == 100 then likeness * weightExactMatch else likeness * weightFuzzyMatch | (_, _, likeness) <- keywordMatches]
    score = sum keywordMatchesBonus
    matchIndexes = [(t, indx) | (t, indx, _) <- keywordMatches]
  in
    (matchIndexes, score)

computeFrequencyScore :: [Text] -> [Text] -> RankScore
computeFrequencyScore keywords contentWords =
  let
    preliminaryScore = fromIntegral $ 5 * sum (map (keywordFrequency contentWords) keywords)
  in
    preliminaryScore * weightFrequency

-- | Calculate proximity score for different keywords based on their positions. The closer
-- the keywords are to each other, the higher the score. A significant bonus is applied for
-- keywords appearing in the same order with small gaps.
--
-- Assumes @@keywordMatches@@ is supplied in the order keywords were given.
--
-- Example:
-- >>> keywordOrderedProximity [("hello", 0), ("world", 2), ("foo", 5)]
-- 28.57143
keywordOrderedProximity :: [(Text, Int)] -> RankScore
keywordOrderedProximity keywordMatches =
  let positions = map snd keywordMatches
      -- Calculate proximity between different keywords
      keywordPairs = zip positions (tail positions)  -- Consecutive positions for order bonus
      -- Will naturally be the distance between the keywords in the order they were given.
      distances = map (\(i, j) -> abs (i - j)) keywordPairs
      -- Calculate the average distance if there are distances available.
      avgDistanceBonus =
        if length keywordMatches <= 1
          then 0
          else
            let average = fromIntegral (sum distances) / fromIntegral (length distances)
            in (1 / (average + 1))
  in avgDistanceBonus * weightOrderedProximity

-- | Count the number of times a keyword appears in the content
keywordFrequency :: [Text] -> Text -> Int
keywordFrequency contentWords keyword =
  length $ filter (== keyword) contentWords

-- TODO: doesn't seem very forgiving to small word typos
-- | Calculate percentage likeness and index of the *best* (fuzzy) match.
--
-- Highest potential score is 100. As in 100% similarity.
--
-- Example:
-- >>> bestFuzzyMatch "togs" ["do", "you", "like", "tags?"]
-- (60.0,3)
bestFuzzyMatch
    :: Text
    -- ^ Keyword to search for.
    -> [Text]
    -- ^ The words (tokens) to search in.
    -> (Float, Int)
    -- ^ Best likeness percentage and index where it was found.
bestFuzzyMatch keyword contentWords =
  let distances = zipWith (\i word -> (editDistance keyword word, i)) [0 ..] contentWords
      (minDistance, bestMatchIndex) = minimumBy (comparing fst) distances
      maxPossibleDistance = max (T.length keyword) (T.length (contentWords !! bestMatchIndex)) -- maximum distance based on word lengths; also unsafe function
      likenessPercentage = max 0 (100 - (fromIntegral minDistance / fromIntegral maxPossibleDistance) * 100) -- Normalize distance to 100 scale
   in (likenessPercentage, bestMatchIndex)

-- | Helper function to calculate Levenshtein distance using 'Text.EditDistance'
--
-- The distance returned will be between 0 (exact match) and the length of the longest
-- of the two words compared.
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
searchDocuments
  :: [Text]
  -> AbsolutePath
  -> AbsolutePath
  -> IO [(FilePath, String, Bool, RankScore, [Text])]
  -- ^ The file path, the highlighted selector, whether it's a menu, the score, and the context snippets.
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
              (highlightedSelector, score, contexts) = rankDocument relativePath keywords content
              nonOverlappingContexts = combineContexts contexts
              contextTexts = map csText nonOverlappingContexts
           in (fp, highlightedSelector, isMenu, score, contextTexts)
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
  let prunedResults = filter (\(_, _,  _, s, _) -> s >= scoreThreshold) documentResults
  pure $ searchResponse absoluteOutputPath query $ L.sortOn (\(_, _, _, s, _) -> negate s) prunedResults

makeInfoLine :: B.ByteString -> GopherMenuItem
makeInfoLine t = Item InfoLine t "" Nothing Nothing

-- Function to generate a GopherResponse for search results
searchResponse
  :: AbsolutePath
  -> Text
  -> [(FilePath, String, Bool, RankScore, [Text])]
  -- ^ The selector (?) or filepath (?), the highlighted selector, whether it's a menu, the score, and the context snippets.
  -> GopherResponse
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
makeSearchResult
  :: AbsolutePath
  -> (FilePath, String, Bool, RankScore, [Text])
  -- ^ The selector (?) or filepath (?), the highlighted selector, whether it's a menu, the score, and the context snippets.
  -> [GopherMenuItem]
makeSearchResult absoluteOutputPath (fp, highlightedSelector, isMenu, score, contexts) =
  makeLink selector : scoreLine : [makeSummary (T.intercalate " ... " contexts)]
  where
    selector = "/" </> makeRelative absoluteOutputPath fp
    scoreLine = makeInfoLine $ E.encodeUtf8 $ "Score: " <> T.pack (show score)
    makeSummary summary' = makeInfoLine $ E.encodeUtf8 $ summary'
    makeLink selector' =
      Item
        (if isMenu then Directory else File)
        (C8.pack highlightedSelector)
        (C8.pack selector')
        Nothing
        Nothing
