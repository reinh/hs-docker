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
import           Data.Maybe
import           Data.Scientific
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Thyme
import qualified Network.Wreq                   as W
import           Prelude                        hiding (all)
import           Text.PrettyPrint
import           Text.PrettyPrint.GenericPretty

import           Network.Docker.Client.Core

newtype EpochTime = EpochTime { getUTCTime :: UTCTime }
  deriving (Show)

instance Out EpochTime where
  docPrec _ = text . show
  doc = text . show

instance FromJSON EpochTime where
  parseJSON =
    withScientific
      "Created"
      (return . EpochTime . epochToUTC . fromJust . toBoundedInteger)

data Image =
  Image {_imageId          :: String
        ,_imageParentId    :: String
        ,_imageCreated     :: EpochTime
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
          <*> o .: "Created"
          <*> o .: "RepoTags"
          <*> o .: "Size"
          <*> o .: "VirtualSize"

listImages :: Client -> IO (W.Response [Image])
listImages = listImagesWith W.defaults

listImagesWith :: W.Options -> Client ->  IO (W.Response [Image])
listImagesWith opts c = W.asJSON =<< getWith opts c "images/json"
