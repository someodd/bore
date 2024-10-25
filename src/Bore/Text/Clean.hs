-- | Sanitation and cleaning of text data.
--
-- Useful because a lot of gopher content has ASCII art and other non-essential data, so
-- if we want to get to the important bits it can be tricky.
module Bore.Text.Clean where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isPunctuation, isAlphaNum, isSpace, generalCategory, GeneralCategory(..))

-- | Remove repeating punctuation and keep only desired characters (letters, numbers, punctuation, and spaces).
cleanText :: Text -> Text
cleanText = removeRepeatingPunctuation . removeNonStandardChars

-- | Remove non-standard characters, only keeping Chinese, Latin, numbers, punctuation, and spaces.
removeNonStandardChars :: Text -> Text
removeNonStandardChars = T.filter isValidChar

-- | Check if a character is valid (Latin alphabet, Chinese characters, numbers, or spaces).
isValidChar :: Char -> Bool
isValidChar c
  | isAlphaNum c = True  -- Keep Latin alphabet and numbers
  | isPunctuation c = True  -- Keep punctuation
  | generalCategory c == OtherLetter = True  -- Keep Chinese characters
  | isSpace c = True  -- Keep spaces
  | otherwise = False  -- Remove all other characters

-- | Remove repeating punctuation by collapsing consecutive punctuations into a single one.
removeRepeatingPunctuation :: Text -> Text
removeRepeatingPunctuation = T.concat . collapsePunctuation . T.group

-- | Helper function to collapse groups of repeating punctuation marks into one.
collapsePunctuation :: [Text] -> [Text]
collapsePunctuation = map collapseGroup
  where
    collapseGroup grp
      | isPunctuation (T.head grp) = T.take 1 grp  -- If group is all punctuation, keep just one
      | otherwise = grp  -- Otherwise keep the group as is
