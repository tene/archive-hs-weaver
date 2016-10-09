{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Weaver where

import           Control.Concurrent           (readChan, threadDelay, writeChan)
import           Control.Exception.Safe
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import qualified Data.Conduit.Combinators     as DCC
import           Data.Conduit.Network.Unix    (AppDataUnix, appSink, appSource,
                                               clientSettings, runUnixClient)
import           Data.Store
import           Data.Store.Streaming
import           Data.Text
import           GHC.Generics                 (Generic)
import           System.Directory             (getHomeDirectory)
import           System.FilePath              ((</>))
import           System.IO
import           System.Posix.Process         (getProcessID)
import qualified System.Process               as P

launchShellProcess :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
launchShellProcess shellCommandText =  P.createProcess (P.shell shellCommandText){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe, P.std_err = P.CreatePipe}

type WeaverEventSource = Source (ResourceT IO) WeaverEvent
type WeaverRequestSink = Sink WeaverRequest IO ()
type WeaverClient = WeaverEventSource -> WeaverRequestSink -> IO ()

-- TODO this logic for detecting if we need to launch a process is
-- definitely wrong; we should just check if we can connect to the socket
weaverConnect :: Maybe String -> WeaverClient -> IO ()
weaverConnect name thread = do
  path <- getWeaverSocketPath name'
  catchAny
    (runUnixClient (clientSettings path) _clientContext)
    spawn_first
  where
    _clientContext app =
      let request_sink = DCC.map Message .| conduitEncode .| appSink app
          event_source = appSource app .| conduitDecode Nothing .| DCC.map fromMessage
      in thread event_source request_sink
    spawn_first ex = do
      putStrLn "weaverd appears to not be already running; launching it"
      startWeaverServer name'
      weaverConnect name thread
    name' = defaultSocketName name

defaultSocketName = maybe "weaver" id

sourceChan x = DCC.repeatM $ readChan x
sinkChan x = DCC.mapM_ (liftIO . writeChan x)

-- TODO this logic for detecting if launching the process is successful is
-- definitely wrong; we should just check if we can connect to the socket
startWeaverServer name = do
  ph <- P.spawnProcess "weaverd" [name]
  threadDelay 500000
  ec <- P.getProcessExitCode ph
  case ec of
    Nothing -> return ph
    _ -> error "Failure launching weaverd!"

getWeaverSocketPath name = do
  home <- getHomeDirectory
  return $ home </> ".weaver" </> name ++ ".socket"


getWeaverSocketPaths = do
  home <- getHomeDirectory
  return [home </> ".weaver" </> "weaver.socket"]

debug_dump :: (MonadIO m, Show a) => Sink a m ()
debug_dump = awaitForever $ liftIO . print

data WeaverRequest
  = Hello String
  | RunShellCommand String
  | RunCommand [Text]
  | KillProcess ProcessId
  | SendInput ProcessId [Text]
  deriving (Show, Generic)

instance Store WeaverRequest

newtype ProcessId = ProcessId { getProcessId :: Int } deriving (Show, Generic)

instance Store ProcessId

data WeaverProcess = WeaverProcess {
    processId     :: ProcessId
  , processName   :: String
  , processOutput :: BS.ByteString
} deriving (Show, Generic)

instance Store WeaverProcess

data WeaverEvent
  = Goodbye String
  | ProcessLaunched WeaverProcess
  | ProcessOutput ProcessId BS.ByteString
  | ProcessTerminated ProcessId Integer
  | WDebug String --- TODO add debug levels
  deriving (Show, Generic)

instance Store WeaverEvent
