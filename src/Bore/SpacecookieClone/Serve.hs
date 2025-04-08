{- | Copied from the spacecookie project, with some modifications, mostly
to avoid having to build it from scratch in deployment environments.

https://github.com/sternenseemann/spacecookie/

-}
{-# LANGUAGE OverloadedStrings #-}
module Bore.SpacecookieClone.Serve (runServerWithConfig) where

import Venusia.Server
import Venusia.FileHandler
import Venusia.MenuBuilder

import Bore.SpacecookieClone.Search.Search
import qualified Bore.Config as BoreConfig
import Bore.FileLayout

import Data.Text.Encoding (decodeUtf8)
import Network.Gopher
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Maybe (fromMaybe)
import System.Posix.Directory (changeWorkingDirectory)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)
import Network.Gopher.Util (asciiOrd)

-- temporary until we start using ryvm
fileTypeToChar :: GopherFileType -> Word8
fileTypeToChar t = asciiOrd $
  case t of
    File -> '0'
    Directory -> '1'
    PhoneBookServer -> '2'
    Error -> '3'
    BinHexMacintoshFile -> '4'
    DOSArchive -> '5'
    UnixUuencodedFile -> '6'
    IndexSearchServer -> '7'
    TelnetSession -> '8'
    BinaryFile -> '9'
    RedundantServer -> '+'
    Tn3270Session -> 'T'
    GifFile -> 'g'
    ImageFile -> 'I'
    InfoLine -> 'i'
    Html -> 'h'

-- | Temporary until we start using ryvm
response :: T.Text -> Int -> GopherResponse -> IO Response
response _ _ (FileResponse str) = pure $ TextResponse (decodeUtf8 str)
response _ _ (ErrorResponse reason) = pure $ TextResponse $ menu [error' (decodeUtf8 reason)]
response hostDefault portDefault (MenuResponse items) = do
  let appendItem acc (Item fileType title path host port) =
        acc <> BB.word8 (fileTypeToChar fileType) <> mconcat
          [ BB.byteString title
          , BB.charUtf8 '\t'
          , BB.byteString path
          , BB.charUtf8 '\t'
          , BB.byteString (fromMaybe (encodeUtf8 hostDefault) host)
          , BB.charUtf8 '\t'
          , BB.intDec . fromIntegral $ fromMaybe (fromIntegral portDefault) port
          , BB.byteString "\r\n"
          ]
  let wholeMenu = foldl appendItem mempty items
  let menuAsText = decodeUtf8 $ B.concat . BSL.toChunks $ BB.toLazyByteString wholeMenu
  pure $ TextResponse menuAsText

-- | Register your routes.
routes :: AbsolutePath -> AbsolutePath -> FilePath -> T.Text -> Int -> [Route]
routes sourceDirectoryAbsolutePath absoluteOutputPath serveRootOnDisk host port =
  [ on "/search" (handleSearch sourceDirectoryAbsolutePath absoluteOutputPath host port)
  , onWildcard "*" $ \request ->
      case request.reqWildcard of
        Just wildcard -> serveDirectory host port serveRootOnDisk "/" wildcard
        Nothing -> pure $ TextResponse "No wildcard provided."
  ]

-- | Handler for search queries (Gopher item type 7).
handleSearch :: AbsolutePath -> AbsolutePath -> T.Text -> Int -> Request -> IO Response
handleSearch sourceDirectoryAbsolutePath absoluteOutputPath host port request = do
  let query =
        case request.reqQuery of
          Nothing -> ""
          (Just something) -> something
  results <- getSearchResults query sourceDirectoryAbsolutePath absoluteOutputPath
  response host port results

runServerWithConfig :: BoreConfig.ServerConfig -> AbsolutePath -> AbsolutePath -> IO ()
runServerWithConfig boreConfig sourceDirectoryAbsolutePath absoluteOutputPath = do
  changeWorkingDirectory absoluteOutputPath
  let
    port = (fromIntegral $ fromMaybe 70 boreConfig.listenPort :: Int)
    address = (fromMaybe "localhost" boreConfig.listenAddress)
  putStrLn . T.unpack $ "Starting server on " <> address <> ":" <> T.pack (show port)
  serve
    (show port)
    noMatchHandler
    (routes sourceDirectoryAbsolutePath absoluteOutputPath absoluteOutputPath boreConfig.hostname port)