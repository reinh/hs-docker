{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Docker.Client.Images where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.List.Split
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Thyme
import qualified Network.Wreq                   as W
import           Prelude                        hiding (all)
import           Text.PrettyPrint.GenericPretty

import           Network.Docker.Client.Core

data Image =
  Image {_imageId          :: String
        ,_imageParentId    :: String
        ,_imageCreated     :: UTCTime
        ,_imageRepoTags    :: [String]
        ,_imageSize        :: Int
        ,_imageVirtualSize :: Int}
  deriving (Show,Generic)

makeLenses ''Image

instance HasId Image where
  id' = imageId

instance Out Image

instance FromJSON Image where
  parseJSON = withObject "Image" $ \o ->
    Image <$> o .: "Id"
          <*> o .: "ParentId"
          <*> (epochToUTC <$> o .: "Created")
          <*> o .: "RepoTags"
          <*> o .: "Size"
          <*> o .: "VirtualSize"

images :: Client -> IO (W.Response [Image])
images = imagesWith W.defaults

imagesWith :: W.Options -> Client ->  IO (W.Response [Image])
imagesWith opts c = W.asJSON =<< getWith opts c "images/json"
