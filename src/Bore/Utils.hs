{- | Miscellaneous tools that are pretty loosely coupled, not very specific to the
application.

I may change this into a `.hls` file.

-}

module Bore.Utils (genericToPairs) where

import Data.Data (Data, gmapQ, toConstr, cast)
import Data.Maybe (catMaybes)
import Data.Data (constrFields)
import qualified Data.Text as Text

{- | Convert any generic data type with record syntax to a list
of key-value `Text` pairs.

Uses some Haskell dark magic.

Example:

>>> data Example = Example { field1 :: Text.Text, field2 :: Text.Text }
>>> genericToPairs (Example "value1" "value2")
[("field1", "value1"), ("field2", "value2")]

-}
genericToPairs :: (Data a) => a -> [(Text.Text, Text.Text)]
genericToPairs record = catMaybes $ zipWith extractField (map Text.pack fieldNames) fields
  where
    -- Get the list of field names from the data type
    fieldNames = constrFields $ toConstr record
    fields = gmapQ castToMaybe record

    -- Attempt to cast each field to `Maybe`
    castToMaybe :: (Data b) => b -> Maybe (Maybe Text.Text)
    castToMaybe field = cast field :: Maybe (Maybe Text.Text)
    
    -- Safely extract fields of type `Maybe`
    extractField :: Text.Text -> Maybe (Maybe Text.Text) -> Maybe (Text.Text, Text.Text)
    extractField name (Just (Just value)) = Just (name, value)
    extractField _ _ = Nothing