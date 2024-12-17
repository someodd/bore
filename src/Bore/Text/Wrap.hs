{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bore.Text.Wrap where

import Commonmark
import Data.Text (Text)
import qualified Data.Text as T
import Text.Wrap (wrapText, defaultWrapSettings)
import Control.Monad.Reader (Reader, asks, runReader)

-- Configuration for the renderer
data WrapConfig = WrapConfig
  { lineWidth :: Int  -- Configurable line width
  }

-- Custom Renderer for Wrapping Text
newtype WrapRenderer = WrapRenderer { getWrappedText :: Reader WrapConfig Text }

instance Show WrapRenderer where
  show (WrapRenderer t) = show (runReader t (WrapConfig 80))
instance Rangeable WrapRenderer where
  ranged _ (WrapRenderer t) = WrapRenderer t
instance HasAttributes WrapRenderer where
  addAttributes _ (WrapRenderer t) = WrapRenderer t

instance Semigroup WrapRenderer where
  WrapRenderer a <> WrapRenderer b = WrapRenderer $ (<>) <$> a <*> b

instance Monoid WrapRenderer where
  mempty = WrapRenderer $ pure ""

-- Handle Paragraphs
instance IsBlock WrapRenderer WrapRenderer where
  paragraph (WrapRenderer content) = WrapRenderer $ do
    width <- asks lineWidth
    wrapped <- content
    if '\t' `T.elem` wrapped
      then return $ wrapped <> "\n\n"
      else return $ wrapText defaultWrapSettings width wrapped <> "\n\n"
  plain = id
  thematicBreak = WrapRenderer $ return "------------------------------\n"
  blockQuote = id  -- need to wrap this too
  heading _ = id
  codeBlock _ t = WrapRenderer $ return $ "```\n" <> t <> "\n```\n\n"
  rawBlock _ t = WrapRenderer $ return t
  list _ _ items = mconcat items  -- wrap this too
  referenceLinkDefinition _ _ = mempty

-- Handle Inline Elements
instance IsInline WrapRenderer where
  str t = WrapRenderer $ return t
  softBreak = WrapRenderer $ return " "
  lineBreak = WrapRenderer $ return "\n"
  emph (WrapRenderer t) = WrapRenderer $ fmap ("*" <>) (fmap (<> "*") t)
  strong (WrapRenderer t) = WrapRenderer $ fmap ("**" <>) (fmap (<> "**") t)
  code t = WrapRenderer $ return $ "`" <> t <> "`"
  link _ _ (WrapRenderer t) = WrapRenderer t
  image _ _ (WrapRenderer t) = WrapRenderer t
  escapedChar c = WrapRenderer $ return $ T.singleton c
  entity t = WrapRenderer $ return t
  rawInline _ rawContent = WrapRenderer $ return rawContent

-- Parse and Render Markdown
wrapMarkdownParagraphs :: Int -> Text -> Either String Text
wrapMarkdownParagraphs width input =
  case commonmark "source" input of
    Left err -> Left $ show err
    Right (WrapRenderer result) ->
      Right $ runReader result (WrapConfig width)

wrapMarkdown :: Int -> Text -> Text
wrapMarkdown width input =
  case wrapMarkdownParagraphs width input of
    Left err -> "Error: " <> T.pack err
    Right output -> output