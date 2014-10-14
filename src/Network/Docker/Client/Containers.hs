{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Docker.Client.Containers where

import           Control.Applicative
import           Control.Lens                   hiding ((.=))
import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import           Data.Default
import           Data.List.Split
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Thyme
import           Network.Docker.Client.Core
import qualified Network.Wreq                   as W
import           Network.Wreq.Types             (Postable, postPayload)
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

listContainers :: Client -> IO (W.Response [Container])
listContainers = listContainersWith W.defaults

listContainersWith :: W.Options -> Client ->  IO (W.Response [Container])
listContainersWith opts c  = W.asJSON =<< getWith opts c "containers/json"

data CreateRequest =
  CreateRequest {_createHostname        :: String
                ,_createDomainname      :: String
                ,_createUser            :: String
                ,_createMemory          :: Int
                ,_createMemorySwap      :: Int
                ,_createCpuShares       :: Int
                ,_createCpuset          :: String
                ,_createAttachStdin     :: Bool
                ,_createAttachStdout    :: Bool
                ,_createAttachStderr    :: Bool
                ,_createPortSpecs       :: Maybe String
                ,_createExposedPorts    :: [(String,String)]
                ,_createTty             :: Bool
                ,_createOpenStdin       :: Bool
                ,_createStdinOnce       :: Bool
                ,_createEnv             :: [String]
                ,_createCmd             :: [String]
                ,_createImage           :: String
                ,_createVolumes         :: [(String,String)]
                ,_createWorkingDir      :: String
                ,_createEntrypoint      :: Maybe String
                ,_createNetworkDisabled :: Bool
                ,_createOnBuild         :: Maybe String}
  deriving (Show,Generic)

makeLenses ''CreateRequest

instance Default CreateRequest where
  def =
    CreateRequest {_createHostname        = ""
                  ,_createDomainname      = ""
                  ,_createUser            = ""
                  ,_createMemory          = 0
                  ,_createMemorySwap      = 0
                  ,_createCpuShares       = 0
                  ,_createCpuset          = ""
                  ,_createAttachStdin     = False
                  ,_createAttachStdout    = False
                  ,_createAttachStderr    = False
                  ,_createPortSpecs       = Nothing
                  ,_createExposedPorts    = []
                  ,_createTty             = False
                  ,_createOpenStdin       = False
                  ,_createStdinOnce       = False
                  ,_createEnv             = []
                  ,_createCmd             = []
                  ,_createImage           = ""
                  ,_createVolumes         = []
                  ,_createWorkingDir      = ""
                  ,_createEntrypoint      = Nothing
                  ,_createNetworkDisabled = False
                  ,_createOnBuild         = Nothing}


instance ToJSON CreateRequest where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 7
                                         ,omitNothingFields = False})

instance Postable CreateRequest where
  postPayload = postPayload . toJSON

data CreateResponse =
  CreateResponse {_createResponseId       :: String
                 ,_createResponseWarnings :: Maybe [String]}
  deriving (Show,Generic)

makeLenses ''CreateResponse

instance FromJSON CreateResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 15})

createContainer :: Client -> W.Options -> CreateRequest -> IO (W.Response CreateResponse)
createContainer c opts payload = W.asJSON =<< postWith opts c "containers/create" payload

data StatusCode = StatusCode Int
  deriving (Show, Generic)

instance FromJSON StatusCode where
  parseJSON =
    withObject "StatusCode" $
    \o -> StatusCode <$> o .: "StatusCode"

waitContainer :: Client -> String -> W.Options -> IO (W.Response StatusCode)
waitContainer c id' opts = W.asJSON =<< postWith opts c ("containers" </> id' </> "wait") BS.empty
