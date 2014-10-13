{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Docker.Client.Misc
  ( info
  , ping
  , version
  ) where

import           Data.ByteString.Lazy       (ByteString)
import qualified Network.Wreq               as W

import           Network.Docker.Client.Core

info, ping, version :: Client -> IO (W.Response ByteString)
info c = get c "info"
ping c = get c "_ping"
version c = get c "version"
