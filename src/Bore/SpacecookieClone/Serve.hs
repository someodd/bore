{- | Copied from the spacecookie project, with some modifications, mostly
to avoid having to build it from scratch in deployment environments.

https://github.com/sternenseemann/spacecookie/

-}
{-# LANGUAGE OverloadedStrings #-}
module Bore.SpacecookieClone.Serve where

import Bore.SpacecookieClone.Search.Search
import Bore.SpacecookieClone.Config
import Bore.SpacecookieClone.FileType
import Bore.SpacecookieClone.Systemd
import qualified Bore.Config as BoreConfig
import Bore.SpacecookieClone.Config as SpaceConfig
import Bore.FileLayout

import Data.Text.Encoding (decodeUtf8)
import Network.Gopher
import Network.Gopher.Util (sanitizePath, boolToMaybe, dropPrivileges)
import Network.Gopher.Util.Gophermap
import qualified Data.ByteString as B
import Control.Applicative ((<|>))
import Control.Exception (catches, Handler (..))
import Control.Monad (when)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bifunctor (first)
import Data.ByteString.Builder (Builder ())
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Exit
import System.FilePath.Posix.ByteString ( RawFilePath, takeFileName, (</>)
                                        , dropDrive, decodeFilePath
                                        , encodeFilePath)
import qualified System.Log.FastLogger as FL
import System.Posix.Directory (changeWorkingDirectory)
import System.Socket (SocketException ())
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

{- | Convert a config from Bore to the config Spacecookie expects.

-}
boreConfigToConfig :: BoreConfig.ServerConfig -> AbsolutePath -> IO SpaceConfig.Config
boreConfigToConfig boreConfig absoluteOutputPath = do
  pure $ SpaceConfig.Config
    { serverName = encodeUtf8 $ boreConfig.hostname
    , runUserName = T.unpack <$> boreConfig.user
    , rootDirectory = absoluteOutputPath
    , listenAddr = maybe (Just "::") (Just . encodeUtf8) boreConfig.listenAddress
    , serverPort = fromMaybe 70 (fromIntegral <$> boreConfig.listenPort)
    , logConfig = SpaceConfig.LogConfig
        { logEnable = True
        , logLevel = GopherLogLevelInfo
        , logHideTime = False
        , logHideIps = False
        }
    }

runServerWithConfig :: BoreConfig.ServerConfig -> AbsolutePath -> AbsolutePath -> IO ()
runServerWithConfig boreConfig sourceDirectoryAbsolutePath absoluteOutputPath = do
  config <- boreConfigToConfig boreConfig absoluteOutputPath
  changeWorkingDirectory (rootDirectory config)
  (logHandler, logStopAction) <- fromMaybe (Nothing, pure ())
    . fmap (first Just) <$> makeLogHandler (logConfig config)
  let cfg = GopherConfig
        { cServerName = serverName config
        , cListenAddr = listenAddr config
        , cServerPort = serverPort config
        , cLogHandler = logHandler
        }
      logIO = fromMaybe noLog logHandler

  let setupFailureHandler e = do
        logIO GopherLogLevelError
          $  "Exception occurred in setup step: "
          <> toGopherLogStr (show e)
        logStopAction
        exitFailure
      catchSetupFailure a = a `catches`
        [ Handler (setupFailureHandler :: SystemdException -> IO ())
        , Handler (setupFailureHandler :: SocketException -> IO ())
        ]

  catchSetupFailure $ runGopherManual
    (systemdSocket cfg)
    (afterSocketSetup logIO config)
    (\s -> do
      _ <- notifyStopping
      logStopAction
      systemdStoreOrClose s)
    cfg
    (spacecookie sourceDirectoryAbsolutePath absoluteOutputPath logIO)

afterSocketSetup :: GopherLogHandler -> Config -> IO ()
afterSocketSetup logIO cfg = do
  case runUserName cfg of
    Nothing -> pure ()
    Just u  -> do
      dropPrivileges u
      logIO GopherLogLevelInfo $ "Changed to user " <> toGopherLogStr u
  _ <- notifyReady
  pure ()

makeLogHandler :: LogConfig -> IO (Maybe (GopherLogHandler, IO ()))
makeLogHandler lc =
  let wrapTimedLogger :: FL.TimedFastLogger -> FL.FastLogger
      wrapTimedLogger logger str = logger $ (\t ->
        "[" <> FL.toLogStr t <> "]" <> str)
      formatLevel lvl =
        case lvl of
          GopherLogLevelInfo  -> "[info] "
          GopherLogLevelWarn  -> "[warn] "
          GopherLogLevelError -> "[err ] "
      processMsg =
        if logHideIps lc
          then hideSensitive
          else id
      logHandler :: FL.FastLogger -> GopherLogLevel -> GopherLogStr -> IO ()
      logHandler logger lvl msg = when (lvl <= logLevel lc) . logger
        $  formatLevel lvl
        <> ((FL.toLogStr :: Builder -> FL.LogStr) . fromGopherLogStr . processMsg $ msg)
        <> "\n"
      logType = FL.LogStderr FL.defaultBufSize
   in sequenceA . boolToMaybe (logEnable lc) $ do
     (logger, cleanup) <-
       if logHideTime lc
         then FL.newFastLogger logType
         else first wrapTimedLogger <$> do
           timeCache <- FL.newTimeCache FL.simpleTimeFormat
           FL.newTimedFastLogger timeCache logType
     pure (logHandler logger, cleanup)

noLog :: GopherLogHandler
noLog = const . const $ pure ()

-- Function to handle search functionality
spacecookie :: AbsolutePath -> AbsolutePath -> GopherLogHandler -> GopherRequest -> IO GopherResponse
spacecookie sourceDirectoryAbsolutePath absoluteOutputPath logger req = do
  let selector = requestSelector req
      path = "." </> dropDrive (sanitizePath selector)

  -- this could be updated to be nicer by using gopher request data type
  -- Handle /search selector
  case requestSearchString req of
    Just searchString ->
      if "/search" == selector
        then do
          let query = T.strip $ decodeUtf8 searchString
          getSearchResults query sourceDirectoryAbsolutePath absoluteOutputPath
        else do
          -- error, not supported endpoint
          pure $ ErrorResponse "You can only perform searches on /search"
    Nothing -> do
      pt <- gopherFileType path

      case pt of
        Left PathIsNotAllowed ->
          pure . ErrorResponse $ mconcat
            [ "Accessing '",  selector, "' is not allowed." ]
        Left PathDoesNotExist -> pure $
          if "URL:" `B.isPrefixOf` selector
            then ErrorResponse $ mconcat
              [ "spacecookie does not support proxying HTTP, "
              , "try using a gopher client that supports URL: selectors. "
              , "If you tried to request a resource called '"
              , selector, "', it does not exist." ]
            else ErrorResponse $ mconcat
              [ "The requested resource '", selector
              , "' does not exist or is not available." ]
        Right ft ->
          case ft of
            Error -> pure $ ErrorResponse $ "An unknown error occurred"
            Directory -> gophermapResponse logger path
            _ -> fileResponse logger path

fileResponse :: GopherLogHandler -> RawFilePath -> IO GopherResponse
fileResponse _ path = FileResponse <$> B.readFile (decodeFilePath path)

makeAbsolute :: RawFilePath -> RawFilePath
makeAbsolute x = fromMaybe x
  $   boolToMaybe ("./" `B.isPrefixOf` x) (B.tail x)
  <|> boolToMaybe ("." == x) "/"

directoryResponse :: GopherLogHandler -> RawFilePath -> IO GopherResponse
directoryResponse _ path =
  let makeItem :: Either a GopherFileType -> RawFilePath -> Either a GopherMenuItem
      makeItem t file = do
        fileType <- t
        pure $
          Item fileType (takeFileName file) file Nothing Nothing
   in do
     dir <- map ((path </>) . encodeFilePath)
       <$> getDirectoryContents (decodeFilePath path)
     fileTypes <- mapM gopherFileType dir

     pure . MenuResponse . rights
       $ zipWith makeItem fileTypes (map makeAbsolute dir)

gophermapResponse :: GopherLogHandler -> RawFilePath -> IO GopherResponse
gophermapResponse logger path = do
  let gophermap = path </> ".gophermap"
      gophermapWide = decodeFilePath gophermap
  exists <- doesFileExist gophermapWide
  parsed <-
    if exists
      then parseOnly parseGophermap <$> B.readFile gophermapWide
      else pure $ Left "Gophermap file does not exist"
  case parsed of
    Left err -> do
      when exists . logger GopherLogLevelWarn
        $  "Could not parse gophermap at " <> toGopherLogStr gophermap
        <> ": " <> toGopherLogStr err
      directoryResponse logger path
    Right right -> pure
      $ gophermapToDirectoryResponse (makeAbsolute path) right
