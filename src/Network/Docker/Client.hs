{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Docker.Client
  ( go
  , module Network.Docker.Client.Containers
  , module Network.Docker.Client.Core
  , module Network.Docker.Client.Images
  , module Network.Docker.Client.Misc
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Data.Aeson
import           Data.ByteString.Lazy             (ByteString)
import           Data.List.Split
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Network.Wreq                     as W
import           Prelude                          hiding (all)
import           System.Environment
import           Text.PrettyPrint.GenericPretty

import           Network.Docker.Client.Containers
import           Network.Docker.Client.Core
import           Network.Docker.Client.Images
import           Network.Docker.Client.Misc

go =
  do host <- lookupEnv "DOCKER_HOST"
     let c = Client (fromMaybe "http://192.168.59.104:2375" host)
     r <- containersWith (W.defaults & W.param "all" .~ value True) c
     mapM_ pp (r ^. W.responseBody)
     r <- images c
     mapM_ pp (r ^. W.responseBody)
