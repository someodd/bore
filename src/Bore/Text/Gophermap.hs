-- FIXME: it is probably better to not have this and to instead create a
-- .gophermap file in a new directory that matches the name of the file.
--
-- Keep this around?
{- | Take a file formatted according to a specific gophermap spec and transform it into
something that can be served as-is (legitimate gopher menu according to RFC1436,
basically).

For reference: https://sternenseemann.github.io/spacecookie/spacecookie.gophermap.5.html

Create a simple Haskell function which will transform a regular text file into a gopher
menu intended to be served as-is in gopherspace (use Text data type) according to this
manpage (which indicates how the text file can be formatted before it is parsed by this
Haskell function into a regular gopher menu according to the gopher protocol menu
specification)

-}

module Bore.Text.Gophermap (toGophermap) where

import Data.Text (Text)
import qualified Data.Text as T

{- | Transform a regular text file into a Gopher menu.

-}
toGophermap :: Text -> Int -> Text -> Text
toGophermap domain port inputText = (T.unlines . map processLine . T.lines $ inputText) <> "\n\n"
  where
    textPort = T.pack . show $ port

    processLine :: Text -> Text
    processLine line
      -- Info line: no tabs and not starting with a recognized gopher file type.
      -- also weirdly a label must at least have one character for .gophermap in spacecookie to parse, so let's just add it
      | not (isMenuEntry line) = T.intercalate "\t" ["i" <> if T.null line then " " else line, "/", domain, textPort]
      -- Menu entry: ensure the last fields (domain and port) are present.
      | otherwise = addDomainAndPort line

    -- Determine if a line is a menu entry based on the first character.
    isMenuEntry :: Text -> Bool
    isMenuEntry t = not (T.null t) && T.head t `elem` gopherFileTypes && "\t" `T.isInfixOf` t

    -- Gopher file types as per RFC1436 and spacecookie.
    gopherFileTypes :: [Char]
    gopherFileTypes = "0123456789+gITi"

    -- Add domain and port if they're missing from the line.
    addDomainAndPort :: Text -> Text
    addDomainAndPort line =
      let (fileTypeChar, rest) = T.splitAt 1 line
          fields = T.splitOn "\t" rest
      in case fields of
           -- If there are fewer than 3 fields, add domain and port.
           [name] -> T.intercalate "\t" [fileTypeChar <> name, name, domain, textPort]
           [name, selector] -> T.intercalate "\t" [fileTypeChar <> name, selector, domain, textPort]
           [name, selector, server] -> T.intercalate "\t" [fileTypeChar <> name, selector, server, textPort]
           _ -> line -- If all fields are present, return line unchanged.