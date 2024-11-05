{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provable/verified parts of the search module/algo.

LiquidHaskell seems to prefer String over Text.

-}
module Bore.SpacecookieClone.Search.Verified (findKeywordMatches) where

import Bore.SpacecookieClone.Search.WeightsTypes

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.List (foldl', elemIndices)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

{-@ type Percentage = { v:Float | 0 <= v && v <= 100 } @-}
{-@ type NonNegative = { v:Int   | v >= 0 } @-}

-- | Calculate percentage likeness and index of the *best* (fuzzy) match.
--
-- Highest potential score is 100, meaning an exact match.
--
-- Example:
--
-- >>> bestFuzzyMatch "tags" ["do", "you", "like", "tags?"]
-- (80.0, 3)
--
-- >>> bestFuzzyMatch "hello" []
-- (0.0, -1)
{-@ bestFuzzyMatch :: Text -> [Text] -> (Percentage, Int) @-}
bestFuzzyMatch :: Text -> [Text] -> (Float, Int)
bestFuzzyMatch keyword contentWords =
    let indexedWords = zip contentWords [0 ..]
        initialBest = (0.0, -1)  -- Start with 0% likeness and invalid index
        (bestLikeness, bestIndex) = foldl' compareLikeness initialBest indexedWords
    in (bestLikeness, bestIndex)
  where
    {-@ compareLikeness :: (Percentage, Int) -> (Text, Int) -> (Percentage, Int) @-}
    compareLikeness :: (Float, Int) -> (Text, Int) -> (Float, Int)
    compareLikeness (currentBest, currentIndex) (word, idx) =
        let likeness = calculateLikeness keyword word
        in if likeness > currentBest
           then (likeness, idx)
           else (currentBest, currentIndex)

    {-@ calculateLikeness :: Text -> Text -> Percentage @-}
    calculateLikeness :: Text -> Text -> Float
    calculateLikeness k w =
        let dist = editDistance k w
            maxLen = max (T.length k) (T.length w)
            likeness = if maxLen == 0
                       then 100.0
                       else (1.0 - fromIntegral dist / fromIntegral maxLen) * 100.0
        in max 0.0 (min 100.0 likeness)

    -- | Helper function to calculate Levenshtein distance.
    -- We assume this function returns a non-negative integer.
    {-@ assume editDistance :: Text -> Text -> NonNegative @-}
    editDistance :: Text -> Text -> Int
    editDistance k w = levenshteinDistance defaultEditCosts (unpack k) (unpack w)

-- | Find keyword matches (both exact and fuzzy) in the document and their positions.
--
-- The main interface for finding matches for keywords..
--
-- Finds all exact matches, but if there are none, try to find the best fuzzy match. May
-- return multiple matches per keyword.
--
-- Will skip fuzzy matches if the keyword is too short.
--
-- Examples:
-- >>> findKeywordMatches ["ukfnb","kxx","aig","vv","wsx","kd","is","rehvnelfw","silfmxrh","onpviev"] ["ukfnb","kxx","aig","vv","wsx","kd","is","rehvnelfw","silfmxrh","onpviev"]
-- [("ukfnb",0,100.0),("kxx",1,100.0),("aig",2,100.0),("vv",3,100.0),("wsx",4,100.0),("kd",5,100.0),("is",6,100.0),("rehvnelfw",7,100.0),("silfmxrh",8,100.0),("onpviev",9,100.0)]
findKeywordMatches
  :: [Text]
  -> [Text]
  -> [KeywordMatch]
  -- ^ The Float is the percentage likeness of the fuzzy match. For exact match it's 100.
findKeywordMatches keywords contentWords =
  concatMap (findMatchesForKeyword keywords contentWords) keywords

-- | Represents a keyword match.
--
-- The text matched, the index in some context it was found at, and the likeness percentage.
{-@ type KeywordMatch = (Text, Int, Percentage) @-}
type KeywordMatch = (Text, Int, Float)

-- | Find matches (along with their index and likeness percentage) to a keyword in the
-- content.
{-@ findMatchesForKeyword :: 
      contentWords:[Text] 
      -> keywords:[Text] 
      -> Text 
      -> result:[KeywordMatch]
  @-}
findMatchesForKeyword :: [Text] -> [Text] -> Text -> [KeywordMatch]
findMatchesForKeyword _ contentWords' keyword =
    case exactMatches keyword contentWords' of
        [] ->
            if T.length keyword >= minFuzzyKeywordLength
            then findBestFuzzyMatch keyword contentWords'
            else []
        matches -> matches

-- | A little helper function to find all the exact matches of a keyword in the content.
--
-- Includes the index of the match(es) and 100% likeness score for each.
{-@ exactMatches :: Text -> contentWords:[Text] -> [(Text, {v:Int | len contentWords > v && v >= 0}, {v:Percentage | v == 100 })]  @-}
exactMatches :: Text -> [Text] -> [KeywordMatch]
exactMatches keyword contentWords = [(keyword, idx, 100) | idx <- elemIndices keyword contentWords]

-- Assuming for this function from another module, which LiquidHaskell is having trouble with.
{-@ assume elemIndices :: Eq a => x:a -> xs:[a] -> [{v:Nat | v < len xs}] @-}

-- | Simply a helper function for `findMatchesForKeyword`.
{-@ findBestFuzzyMatch :: Text -> [Text] -> [KeywordMatch] @-}
findBestFuzzyMatch :: Text -> [Text] -> [KeywordMatch]
findBestFuzzyMatch keyword contentWords' =
    let (likeness, idx) = bestFuzzyMatch keyword contentWords'
    in [(keyword, idx, max 0 (min 100 likeness)) | likeness >= minimumFuzzyLikeness]
