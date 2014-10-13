{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Docker.Client.Containers where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.List.Split
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Thyme
import           Network.Docker.Client.Core
import qualified Network.Wreq                   as W
import           Prelude                        hiding (all)
import           Text.PrettyPrint
import           Text.PrettyPrint.GenericPretty

import           Network.Docker.Client.Core

data Container =
  Container {_containerCommand :: String
            ,_containerCreated :: UTCTime
            ,_containerId      :: String
            ,_containerImage   :: String
            ,_containerNames   :: [String]
            ,_containerStatus  :: String}
  deriving (Show,Generic)

makeLenses ''Container

instance HasId Container where
  id' = containerId

instance Out Container

instance FromJSON Container where
  parseJSON = withObject "Container" $ \o ->
    Container <$> o .: "Command"
              <*> (epochToUTC <$> o .: "Created")
              <*> o .: "Id"
              <*> o .: "Image"
              <*> o .: "Names"
              <*> o .: "Status"

containers :: Client -> IO (W.Response [Container])
containers = containersWith W.defaults

containersWith :: W.Options -> Client ->  IO (W.Response [Container])
containersWith opts c  = W.asJSON =<< getWith opts c "containers/json"
