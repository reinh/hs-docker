{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Docker.Client.Core where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Thyme
import           Data.Thyme.Clock.POSIX
import qualified Network.Wreq                   as W
import           Network.Wreq.Types             (Postable)
import           Text.PrettyPrint
import           Text.PrettyPrint.GenericPretty

data Client = Client {_url :: String}
  deriving (Show,Generic)

getWith :: W.Options -> Client -> String -> IO (W.Response ByteString)
getWith opts (Client url) path = W.getWith opts (url </> path)

get :: Client -> String -> IO (W.Response ByteString)
get c path = getWith W.defaults c path

postWith :: Postable a => W.Options -> Client -> String -> a -> IO (W.Response ByteString)
postWith opts (Client url) path p = W.postWith opts (url </> path) p

post :: Postable a => Client -> String -> a -> IO (W.Response ByteString)
post (Client url) path p = W.postWith W.defaults (url </> path) p

(</>) :: String -> String -> String
(</>) xs ys = xs ++ "/" ++ ys

class ToValue a where
  value :: a -> [Text]

instance ToValue Bool where
  value True = ["1"]
  value False = ["0"]

instance ToValue Int where
  value = return . T.pack . show

instance ToValue a => ToValue (Maybe a) where
  value Nothing = []
  value (Just x) = value x

epochToUTC :: Int -> UTCTime
epochToUTC epoch = fromSeconds epoch ^. from posixTime

instance Out UTCTime where
  docPrec _ = text . show
  doc = text . show

class HasId a where
  id' :: Lens' a String
