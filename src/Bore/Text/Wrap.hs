{- | Wrap text in Markdown format without converting tabs to spaces.

This module has two problems because of the hacky solution:

1. Gopher links have the potential of being wrapped in the middle of the link, which would
   break the link.
2. The tab character is preserved by using a placeholder, which is not ideal.

-}
module Bore.Text.Wrap (wrapMarkdown) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc

-- Define the placeholder
tabPlaceholder :: Text
tabPlaceholder = "\FFFF"

-- Preprocess: Replace tabs with placeholder
preprocess :: Text -> Text
preprocess = T.replace "\t" tabPlaceholder

-- Postprocess: Replace placeholder back to tabs
postprocess :: Text -> Text
postprocess = T.replace tabPlaceholder "\t"

-- | Word wrap a Markdown document without converting tabs to spaces
wrapMarkdown :: Int -> Text -> Text
wrapMarkdown width input =
  let
    preprocessedInput = preprocess input

    result = runPure $ do
      -- Parse the preprocessed Markdown input into Pandoc AST
      pandoc <- readMarkdown def { readerExtensions = pandocExtensions } preprocessedInput

      -- Render the Pandoc AST back to Markdown with wrapping settings
      writeMarkdown def
          { writerWrapText = WrapAuto
          , writerColumns = width
          }
          pandoc

    wrappedOutput = case result of
      Left err -> error $ show err
      Right output -> postprocess output
  in
    wrappedOutput
