{-# LANGUAGE OverloadedStrings #-}

module Bore.Text.Figlet (loadFigletFonts, renderText, FigletCache, FigletFont(..)) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory)
import System.FilePath ((</>), makeRelative)
import Prelude hiding (lines)
import Safe (maximumDef)

-- Data type for Figlet font metadata
data FigletFont = FigletFont
  { hardBlank :: Char
  , height :: Int
  , baseline :: Int
  , maxLength :: Int
  , oldLayout :: Int
  , commentLines :: Int
  , rightToLeft :: Bool
  , endMarker :: Char
  , charMap :: [(Int, [Text])]
  }

{- | Cache of Figlet fonts to avoid reloading over and over.

Relative paths.

-}
type FigletCache = [(FilePath, FigletFont)]

{- | Load all Figlet fonts from a directory to cache.

The `FilePath`s returned are relative to the directory.

-}
loadFigletFonts :: FilePath -> IO FigletCache
loadFigletFonts dir = do
  files <- listDirectory dir
  let relativeFiles = map (makeRelative dir) files
  fonts <- mapM (\file -> do
                    content <- TIO.readFile (dir </> file)
                    return $ parseFiglet content) files
  return $ zip relativeFiles fonts

-- Parse a Figlet font file into FigletFont data structure
parseFiglet :: Text -> FigletFont
parseFiglet content =
  let (headerLine, linesAfterHeader) = case T.lines content of
                                        [] -> error "Empty Figlet font file"
                                        x:xs -> (x, xs)
      header = parseHeader headerLine
      fontData = drop (commentLines header) linesAfterHeader
      charMap = parseCharMap fontData (height header) (endMarker header)
  in header { charMap = charMap }

-- FIXME: this is actually 
-- Parse the Figlet font header
parseHeader :: Text -> FigletFont
parseHeader line =
  let tokens = T.splitOn " " line
      hardBlankChar = T.index (tokens !! 0) 5
      heightVal = readInt (tokens !! 1)
      baselineVal = readInt (tokens !! 2)
      maxLengthVal = readInt (tokens !! 3)
      oldLayoutVal = readInt (tokens !! 4)
      commentLinesVal = readInt (tokens !! 5)
      rtl = if length tokens > 6 then readInt (tokens !! 6) == 1 else False
      endMarker = T.last $ tokens !! 0
  in FigletFont hardBlankChar heightVal baselineVal maxLengthVal oldLayoutVal commentLinesVal rtl endMarker []

-- Parse character map from Figlet font data
parseCharMap :: [Text] -> Int -> Char -> [(Int, [Text])]
parseCharMap lines height endMarker =
  let chunks = chunkList height lines
      parseChunk i chunk = (i, normalizeCharWidth $ parseFigletChar chunk endMarker)
  in zipWith parseChunk [32..126] chunks

-- Parse a single character's Figlet representation
parseFigletChar :: [Text] -> Char -> [Text]
parseFigletChar textLines endMarker = map (cleanLine endMarker) textLines
  where
    -- Remove trailing end markers (@ and $) and any trailing spaces
    cleanLine _ line = T.dropWhileEnd (`elem` ['@', '$', ' ']) line

-- Normalize character widths to align properly without adding extra space
normalizeCharWidth :: [Text] -> [Text]
normalizeCharWidth textLines =
  let maxWidth = maximumDef 0 (map T.length textLines) -- Find the maximum width of lines
  in map (T.justifyLeft maxWidth ' ') textLines -- Pad each line to the max width

-- Render a Figlet font for given text
{-
renderFiglet :: FilePath -> Text -> IO Text
renderFiglet fontPath inputText = do
  -- Open the file with UTF-8 encoding to handle decoding properly
  handle <- openFile fontPath ReadMode
  hSetEncoding handle utf8
  content <- TIO.hGetContents handle
  let font = parseFiglet content
  return $ renderText font inputText
-}

-- Render the input text using the parsed Figlet font
renderText :: FigletFont -> Text -> Text
renderText font input =
  let charLines = map (lookupFigletChar (charMap font)) (T.unpack input)
      height = maximumDef 0 $ map length charLines
      paddedCharLines = map (padLines height) charLines
  in T.unlines $ concatLines paddedCharLines

-- Lookup the Figlet representation of a character, default to space
lookupFigletChar :: [(Int, [Text])] -> Char -> [Text]
lookupFigletChar cmap char = fromMaybe (lookupFigletChar cmap ' ') (lookup (ord char) cmap)

-- FIXME: wait... textLInes ++ ????
-- Pad lines of Figlet characters to align vertically
padLines :: Int -> [Text] -> [Text]
padLines height textLines = textLines ++ replicate (height - length textLines) ""

-- Concatenate Figlet character lines into rows
concatLines :: [[Text]] -> [Text]
concatLines = map T.concat . transpose

-- Transpose a list of lists (like matrix transpose)
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs
  | any null xs = []
  | otherwise = map head xs : transpose (map tail xs)

-- Read an integer from a Text value
readInt :: Text -> Int
readInt = read . T.unpack

-- Break a list into chunks of given size
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (chunk, rest) = splitAt n xs in chunk : chunkList n rest
