{- | Copied from the spacecookie project, with some modifications, mostly
to avoid having to build it from scratch in deployment environments.

https://github.com/sternenseemann/spacecookie/

-}
{-# LANGUAGE OverloadedStrings #-}
module Bore.Serve (runServerWithConfig) where

import Ryvm.Search (getSearchResults, SearchResult(..))
import Venusia.Server
import Venusia.Gateway
import Venusia.FileHandler
import Venusia.MenuBuilder

--import Bore.SpacecookieClone.Search.Search
import qualified Bore.Config as BoreConfig
import Bore.FileLayout
import Bore.FrontMatter ( isGophermap, parseFrontMatter )

import Data.Maybe (fromMaybe)
import System.Posix.Directory (changeWorkingDirectory)
import qualified Data.Text as T
import Data.Text.IO qualified as TIO
import System.FilePath ((</>), makeRelative)


loadFileContent :: FilePath -> IO T.Text
loadFileContent = TIO.readFile

-- FIXME: no need IO
-- Parse the frontmatter of a file and check if it's a gophermap
isGophermapFile :: T.Text -> Bool
isGophermapFile content =
  case parseFrontMatter content of
    Right (frontmatter, _) -> do
      isGophermap frontmatter
    Left _ -> do
      False

response' :: T.Text -> AbsolutePath -> AbsolutePath -> T.Text -> Int -> [SearchResult] -> IO Response
response' query absoluteSourcePath absoluteOutputPath host port results = do
  menuItems <- mapM (resultToMenuItem absoluteSourcePath absoluteOutputPath host port) results
  pure . TextResponse . render $ info ("Results for search: " <>  query) : concat menuItems

-- | Use Venusia.MenuBuilder to construct a line for a gopher menu.
resultToMenuItem :: AbsolutePath -> AbsolutePath -> T.Text -> Int -> SearchResult -> IO [T.Text]
resultToMenuItem absoluteSourcePath absoluteOutputPath host port result = do
  let
    relativePathToFile = makeRelative absoluteOutputPath result.filePath
    pathInSource = absoluteSourcePath </> relativePathToFile
  contents <- loadFileContent pathInSource
  let
    absoluteSelector = "/" <> relativePathToFile
    itemType =
      if isGophermapFile contents
        then '1'
        else '0'
    title = item itemType (T.pack $ "/" </> result.highlightedFilePath) (T.pack absoluteSelector) host port
  pure
    [ info ""
    , title
    , info $ "Rank score: " <> (T.pack . show $ result.score)
    , info result.contexts
    ]

-- | Register your routes.
routes :: AbsolutePath -> AbsolutePath -> FilePath -> T.Text -> Int -> [Route]
routes sourceDirectoryAbsolutePath absoluteOutputPath serveRootOnDisk host port =
  [ on "/search" (handleSearch sourceDirectoryAbsolutePath absoluteOutputPath host port)
  , onWildcard "*" $ \request ->
      case request.reqWildcard of
        Just wildcard -> serveDirectory host port serveRootOnDisk "/" wildcard Nothing
        Nothing -> pure $ TextResponse "No wildcard provided."
  ]

-- | Detect if the text is a gophermap and if so, correct it for searching.
filterGophermaps :: T.Text -> T.Text
filterGophermaps = id

-- | Handler for search queries (Gopher item type 7).
--
-- Includes host and port to help build the menu.
handleSearch :: AbsolutePath -> AbsolutePath -> T.Text -> Int -> Request -> IO Response
handleSearch sourceDirectoryAbsolutePath absoluteOutputPath host port request = do
  let query =
        case request.reqQuery of
          Nothing -> ""
          (Just something) -> something
  results <- getSearchResults (Just filterGophermaps) query sourceDirectoryAbsolutePath absoluteOutputPath
  response' query sourceDirectoryAbsolutePath absoluteOutputPath host port results

runServerWithConfig :: BoreConfig.ServerConfig -> AbsolutePath -> AbsolutePath -> IO ()
runServerWithConfig boreConfig sourceDirectoryAbsolutePath absoluteOutputPath = do
  changeWorkingDirectory absoluteOutputPath
  let
    port = (fromIntegral $ fromMaybe 70 boreConfig.listenPort :: Int)
    address = (fromMaybe "localhost" boreConfig.listenAddress)
  putStrLn . T.unpack $ "Starting server on " <> address <> ":" <> T.pack (show port)
  gatewayRoutes <- loadGatewayRoutes $ sourceDirectoryAbsolutePath </> "gateways.toml"
  serve
    (show port)
    noMatchHandler
    (gatewayRoutes ++ routes sourceDirectoryAbsolutePath absoluteOutputPath absoluteOutputPath boreConfig.hostname port)