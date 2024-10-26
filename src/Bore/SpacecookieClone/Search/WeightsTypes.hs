module Bore.SpacecookieClone.Search.WeightsTypes where

import Data.Text (Text)

-- | The relevancy score of a document as it pertains to some set of keywords.
type RankScore = Float

-- | Data structure to hold context snippet along with its start and end indices
data ContextSnippet = ContextSnippet
  { csStart :: Int,
    csEnd :: Int,
    csText :: Text
  }
  deriving (Show)

-- | Weight constant for ordered proximity bonus
weightOrderedProximity :: Float
weightOrderedProximity = 1000

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