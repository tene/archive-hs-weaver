{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Weaver where

import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import           Data.Conduit.List            as DCL
import           Data.Conduit.Network.Unix    (AppDataUnix, appSink, appSource,
                                               clientSettings, runUnixClient)
import           Data.Store
import           Data.Store.Streaming
import           Data.Text
import           GHC.Generics                 (Generic)
import           System.Directory             (getHomeDirectory)
import           System.FilePath              ((</>))
import           System.Posix.Process         (getProcessID)

type WeaverEventSource = Source (ResourceT IO) WeaverEvent
type WeaverRequestSink = Sink WeaverRequest IO ()
type WeaverClient = WeaverEventSource -> WeaverRequestSink -> IO ()

weaverConnect :: WeaverClient -> IO ()
weaverConnect thread = do
  paths <- getWeaverSocketPaths
  case paths of
    path:_ -> runUnixClient (clientSettings path) _clientContext
    _      -> error "Failure locating weaver daemon path"
  where
    _clientContext app =
      let request_sink = DCL.map Message =$= conduitEncode =$= appSink app
          event_source = appSource app =$= conduitDecode Nothing =$= DCL.map fromMessage
      in thread event_source request_sink

getWeaverSocketPaths = do
  home <- getHomeDirectory
  return [home </> ".weaver" </> "weaver.socket"]

debug_dump :: (MonadIO m, Show a) => Sink a m ()
debug_dump = awaitForever $ liftIO . print

data WeaverRequest
  = Hello String
  | RunShellCommand Text
  | RunCommand [Text]
  | KillProcess ProcessId
  | SendInput ProcessId [Text]
  deriving (Show, Generic)

instance Store WeaverRequest

newtype ProcessId = ProcessId { getProcessId :: Int } deriving (Show, Generic)

instance Store ProcessId

data WeaverProcess = WeaverProcess {
    processId     :: Int
  , processName   :: Text
  , processOutput :: BS.ByteString
} deriving (Show, Generic)

instance Store WeaverProcess

data WeaverEvent
  = Goodbye String
  | ProcessLaunched WeaverProcess
  | ProcessOutput ProcessId BS.ByteString
  | ProcessTerminated ProcessId Integer
  deriving (Show, Generic)

instance Store WeaverEvent
