{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Markdown wrapper.

## Plans

Should handle links by making them into footnotes, forcing footnotes. This way it can
easily export to jekyll and look right and be nice in gopher, too!

Quickly becoming custom phlog/gopher formatting.

-}
module Bore.Text.Wrap where

import Commonmark
import Data.Text (Text)
import qualified Data.Text as T
import Text.Wrap (wrapText, defaultWrapSettings)
import Control.Monad.Reader (Reader, asks, runReader)

-- Configuration for the renderer
data WrapConfig = WrapConfig
 { lineWidth :: Int -- Configurable line width
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

-- Handle Paragraphs and Other Block Elements
instance IsBlock WrapRenderer WrapRenderer where
 paragraph (WrapRenderer content) = WrapRenderer $ do
   width <- asks lineWidth
   wrapped <- content
   let wrappedNoNewlines = T.replace "\n" " " wrapped
   if '\t' `T.elem` wrappedNoNewlines
     then return $ wrappedNoNewlines <> "\n\n"
     else return $ wrapText defaultWrapSettings width wrappedNoNewlines <> "\n\n"
 
 plain = id
 
 thematicBreak = WrapRenderer $ return "------------------------------\n"
 
 blockQuote (WrapRenderer content) = WrapRenderer $ do
   width <- asks lineWidth
   wrapped <- content
   let wrappedLines = T.lines $ wrapText defaultWrapSettings (width - 2) wrapped
   return $ T.intercalate "\n" (map ("> " <>) wrappedLines) <> "\n\n"
 
 heading level (WrapRenderer content) = WrapRenderer $ do
   wrapped <- content
   return $ T.replicate level "#" <> " " <> wrapped <> "\n\n"
 
 codeBlock _ t = WrapRenderer $ return $ "```\n" <> t <> "```\n\n"
 
 rawBlock _ t = WrapRenderer $ return t
 
 list listType listSpacing items = WrapRenderer $ fmap (<> "\n") $ do
   width <- asks lineWidth
   let 
       wrapListItem item = do
         itemContent <- getWrappedText item
         let indent = T.replicate (listTypeToIndent listType) " "
             wrappedContent = wrapText defaultWrapSettings 
                               (width - (listTypeToIndent listType + 2)) itemContent
             lines' = T.lines wrappedContent
             
             -- Determine list marker based on list type
             marker = case listType of
               BulletList bulletChar -> T.singleton bulletChar <> " "
               OrderedList startNum _ _ -> T.pack (show startNum) <> ". "
         
         return $ T.intercalate "\n" 
           (indent <> marker <> head lines' : 
            map (\l -> indent <> "  " <> l) (tail lines'))
   
   wrappedItems <- mapM wrapListItem items
   return $ T.intercalate "\n" wrappedItems <> 
            (if listSpacing == TightList then "" else "\n") <> "\n"
 
 referenceLinkDefinition _ _ = mempty

-- Helper to convert list type to indentation level
listTypeToIndent :: ListType -> Int
listTypeToIndent (BulletList _) = 0
listTypeToIndent (OrderedList _ _ _) = 0

-- Handle Inline Elements
instance IsInline WrapRenderer where
 str t = WrapRenderer $ return t
 softBreak = WrapRenderer $ return " "
 lineBreak = WrapRenderer $ return "\n"
 emph (WrapRenderer t) = WrapRenderer $ fmap ("*" <>) (fmap (<> "*") t)
 strong (WrapRenderer t) = WrapRenderer $ fmap ("**" <>) (fmap (<> "**") t)
 code t = WrapRenderer $ return $ "`" <> t <> "`"
 link _ _ (WrapRenderer t) = WrapRenderer t
 image _  _ (WrapRenderer t) = WrapRenderer t
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