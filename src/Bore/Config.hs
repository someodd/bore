{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{- | TOML configuration for Bore.

I actually want very minimal configuration, highly opinionated, because I think my lesson
from Burrow was that I was trying to make it too flexible and it was just a pain and I
didn't get a ton out of it as an end user. So I'm just going to carefully introduce
features.

-}
module Bore.Config (Config(..), ServerConfig(..), getConfig) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Toml

-- For Maybe really need to have defaults--maybe i can use a default type
data ServerConfig = ServerConfig
  { hostname :: Text
  , listenAddress :: Maybe Text
  , listenPort :: Maybe Integer
  , user :: Maybe Text
  } deriving (Generic, Show, Eq)

serverConfigCodec :: TomlCodec ServerConfig
serverConfigCodec = Toml.genericCodec

data Config = Config
    { server :: ServerConfig
    }
    deriving (Generic, Show, Eq)

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table serverConfigCodec "server" .= server

getConfig :: FilePath -> IO Config
getConfig configFilePath = Toml.decodeFile configCodec configFilePath