
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Bore.FrontMatter 
    ( FrontMatter(..)
    , isPhlogPost
    , isGophermap
    , parseFrontMatter
    ) where

import GHC.Generics (Generic)
import qualified Data.Text as Text
import Data.Yaml (FromJSON, decodeEither', ParseException(..))
import Data.Maybe (fromMaybe)
import Control.Exception (toException)
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.Data (Data, Typeable)

data FrontMatter = FrontMatter {
    title :: Maybe Text.Text,
    phlog :: Maybe Bool,
    date :: Maybe Text.Text,
    tags :: Maybe [Text.Text],
    draft :: Maybe Bool,
    skipMarkdown :: Maybe Bool,
    skipTemplating :: Maybe Bool,
    gophermap :: Maybe Bool,
    -- ^ indicates that this file should be parsed into a gopher menu
    parent :: Maybe Text.Text,
    -- ^ If this is set, then the document will be rendered as a partial/child of the
    -- defined parent.
    variables :: Maybe (Map.Map Text.Text Text.Text)
    } deriving (Show, Data, Typeable, Generic, FromJSON)

{- | Determines if the frontmatter indicates to parse as a gopher menu/gophermap.
    
-}
isGophermap :: FrontMatter -> Bool
isGophermap frontmatter = fromMaybe False (frontmatter.gophermap)

{- | Determines if the frontmatter indicates a phlog post.

The function `isPhlogPost` takes a `FrontMatter` record and returns a `Bool` indicating whether the `phlog` field in the frontmatter is `True`. If the `phlog` field is `Nothing`, it returns `False`.

Examples:

>>> isPhlogPost (FrontMatter { phlog = Just True })
True

>>> isPhlogPost (FrontMatter { phlog = Just False })
False

>>> isPhlogPost (FrontMatter { phlog = Nothing })
False
-}
isPhlogPost :: FrontMatter -> Bool
isPhlogPost frontmatter = fromMaybe False (frontmatter.phlog)

-- | Function to extract and parse frontmatter from Text
parseFrontMatter :: Text.Text -> Either ParseException (FrontMatter, Text.Text)
parseFrontMatter content =
  case splitFrontmatter content of
    Just (frontmatterText, mainText) -> (, mainText) <$> decodeEither' (TE.encodeUtf8 frontmatterText)
    Nothing -> Left $ OtherParseException $ toException $ userError "No frontmatter found"

-- | Return the frontmatter and the rest of the text as separate Text values.
--
-- Currently is maybe too permissive in that in violation of the spec, I think,
-- it allows for something to precede the frontmatter or for the frontmatter to
-- be empty. There may be weird edge cases here.
splitFrontmatter :: Text.Text -> Maybe (Text.Text, Text.Text)
splitFrontmatter content' =
    let parts = Text.splitOn "---" content'
    in case parts of
        (_:fm:rest) -> Just (fm, Text.intercalate "---" rest)
        _        -> Nothing