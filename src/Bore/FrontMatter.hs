
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Bore.FrontMatter 
    ( FrontMatter(..)
    , isGophermap
    , parseFrontMatter
    , updateFrontMatter
    , splitFrontmatter
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Text.Mustache (ToMustache(..))
import qualified Text.Mustache.Types as MType
import qualified Data.Text as Text
import Data.Yaml (FromJSON, decodeEither', ParseException(..))
import Data.Maybe (fromMaybe)
import Control.Exception (toException)
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.Data (Data, Typeable)
import Control.Applicative ((<|>))

data FrontMatter = FrontMatter {
    title :: Maybe Text.Text,
    date :: Maybe Text.Text,
    tags :: Maybe [Text.Text],
    lambdas :: Maybe [Text.Text],
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

-- FIXME: I hate this.
-- | A function to merge two FrontMatter records, with the second one taking precedence.
updateFrontMatter :: FrontMatter -> FrontMatter -> FrontMatter
updateFrontMatter base FrontMatter{..} = base
  { title = base.title <|> title
  , date = base.date <|> date
  , tags = base.tags <|> tags
  , lambdas = base.lambdas <|> lambdas
  , draft = base.draft <|> draft
  , skipMarkdown = base.skipMarkdown <|> skipMarkdown
  , skipTemplating = base.skipTemplating <|> skipTemplating
  , gophermap = base.gophermap <|> gophermap
  , parent = base.parent <|> parent
  , variables = base.variables <|> variables
  }

-- Automatically derive ToJSON for FrontMatter
instance ToJSON FrontMatter

-- Use mFromJSON to convert FrontMatter to a Mustache Value
instance ToMustache FrontMatter where
  toMustache = MType.mFromJSON

{- | Determines if the frontmatter indicates to parse as a gopher menu/gophermap.
    
-}
isGophermap :: FrontMatter -> Bool
isGophermap frontmatter = fromMaybe False (frontmatter.gophermap)

-- | Function to extract and parse frontmatter from Text
parseFrontMatter :: Text.Text -> Either ParseException (FrontMatter, Text.Text)
parseFrontMatter content =
  case splitFrontmatter content of
    Just (frontmatterText, mainText) -> (, mainText) <$> decodeEither' (TE.encodeUtf8 frontmatterText)
    Nothing -> Left $ OtherParseException $ toException $ userError "No frontmatter found"

-- FIXME: should only split if the document begins with ---\n, while currently this is too permissive.
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