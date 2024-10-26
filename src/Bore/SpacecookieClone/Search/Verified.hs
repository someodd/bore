{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module Bore.SpacecookieClone.Search.Verified where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.List (foldl')
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
