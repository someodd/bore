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
import Control.Monad.State (State, get, modify, runState, gets)
import Data.Maybe (fromMaybe)

import qualified Bore.Library as Library
import Bore.Text.Gophermap (imagePathToGopherType, gopherTypeByExt)
import Bore.Config

-- Configuration for the renderer
data WrapConfig = WrapConfig
 { library :: Library.Library
 -- ^ Assets and config(s) to use for rendering.
 , gopherMap :: Bool
 -- ^ Render as a gophermap? Meaning menu links.
 , lineWidth :: Int
 -- ^ Configurable line width
 }

-- State to hold configuration and footnote data
data RendererState = RendererState
 { config :: WrapConfig       -- Configuration for wrapping
 , footnotes :: [(Text, Text)] -- List of footnotes as (text, URL)
 , nextFnNumber :: Int        -- Next footnote number
 }

newtype WrapRenderer = WrapRenderer { getWrappedText :: State RendererState Text }

-- FIXME: what the heck is going on here?
instance Show WrapRenderer where
 show _ = "Do not use!"

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
   state <- get
   let width = lineWidth (config state)
   wrapped <- content
   let wrappedNoNewlines = T.replace "\n" " " wrapped
   if '\t' `T.elem` wrappedNoNewlines
     then return $ wrappedNoNewlines <> "\n\n"
     else return $ wrapText defaultWrapSettings width wrappedNoNewlines <> "\n\n"

 plain = id

 thematicBreak = WrapRenderer $ return "------------------------------\n"

 blockQuote (WrapRenderer content) = WrapRenderer $ do
   state <- get
   let width = lineWidth (config state)
   wrapped <- content
   let wrappedLines = T.lines $ wrapText defaultWrapSettings (width - 2) wrapped
   return $ T.intercalate "\n" (map ("> " <>) wrappedLines) <> "\n\n"

 heading level (WrapRenderer content) = WrapRenderer $ do
   wrapped <- content
   return $ T.replicate level "#" <> " " <> wrapped <> "\n\n"

 codeBlock _ t = WrapRenderer $ return $ "```\n" <> t <> "```\n\n"

 rawBlock _ t = WrapRenderer $ return t

 list listType listSpacing items = WrapRenderer $ fmap (<> "\n") $ do
   state <- get
   let width = lineWidth (config state)
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
 link url _ (WrapRenderer t) = WrapRenderer $ do
  content <- t
  -- Retrieve current footnote state
  currentFns <- gets footnotes
  fnNumber <- gets nextFnNumber
  -- Append the (text, URL) pair to footnotes and increment the footnote number
  modify $ \s -> s { footnotes = currentFns ++ [(content, url)], nextFnNumber = fnNumber + 1 }
  -- Insert footnote reference
  let footnoteRef = "[" <> T.pack (show fnNumber) <> "]"
  return $ content <> footnoteRef

 -- FIXME
 image url _ (WrapRenderer t) = WrapRenderer $ do
  altText <- t
  let
    imageGopherType = imagePathToGopherType (T.unpack url)
  isGopherMap <- gets (gopherMap . config)
  port <- gets (T.pack . show . fromMaybe 70 . listenPort . server . Library.config . library . config)
  host <- gets (hostname . server . Library.config . library . config)
  if isGopherMap
    then return $ (T.pack [imageGopherType]) <> altText <> "\t" <> url <> "\t" <> host <> "\t" <> port
    else return $ "!" <> altText <> " => " <> url

 escapedChar c = WrapRenderer $ return $ T.singleton c
 entity t = WrapRenderer $ return t
 rawInline _ rawContent = WrapRenderer $ return rawContent

-- TODO/FIXME: add option for parsing as menu (for footnotes and images)
-- Parse and Render Markdown
wrapMarkdownParagraphs :: Library.Library -> Bool -> Int -> Text -> Either String Text
wrapMarkdownParagraphs library gopherMap width input =
 case commonmark "source" input of
   Left err -> Left $ show err
   Right (WrapRenderer result) ->
     let initialState = RendererState (WrapConfig library gopherMap width) [] 1
         (mainText, finalState) = runState result initialState
         footnotesList = footnotes finalState
         numberedFootnotes = zip [1 :: Int ..] footnotesList
         formattedFootnotes = if null footnotesList
                              then ""
                              else "## Footnotes\n\n" <> T.intercalate "\n" (map formatFootnote numberedFootnotes)
     in Right $ mainText <> formattedFootnotes
 where
   -- FIXME: make into links if gopherMap! need way to determine type of gopherlink in the end.
   formatFootnote :: (Int, (Text, Text)) -> Text
   formatFootnote (n, (text, url)) = do
    let
      host = library.config.server.hostname
      port = T.pack . show . fromMaybe 70 $ library.config.server.listenPort
    if gopherMap
      then
        case gopherTypeByExt "text/" $ T.unpack url of
          'h' -> "h" <> "[" <> T.pack (show n) <> "]: " <> text <> ": " <> url <> "\tURL:" <> url <> "\t" <> host <> "\t" <> port
          otherType -> (T.pack [otherType]) <> "[" <> T.pack (show n) <> "]: " <> text <> ": " <> url <> "\t" <> url <> "\t" <> host <> "\t" <> port
      else "[" <> T.pack (show n) <> "]: " <> text <> ": " <> url

{- | Do special things to Markdown for gopherspace.

I mostly made this for phlog articles.

-}
wrapMarkdown
  :: Library.Library
  -- ^ Format the markdown document according to these Assets and config!
  -> Bool
  -- ^ Output a gophermap isntead of just a regular text document.
  -> Int
  -> Text
  -> Text
wrapMarkdown library gopherMap width input =
 case wrapMarkdownParagraphs library gopherMap  width input of
   Left err -> "Error: " <> T.pack err
   Right output -> output
