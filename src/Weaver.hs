{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Weaver where

import qualified Data.ByteString      as BS
import           Data.Store
import           Data.Text
import           GHC.Generics         (Generic)
import           System.Directory     (getHomeDirectory)
import           System.FilePath      ((</>))
import           System.Posix.Process (getProcessID)

getWeaverSocketPaths = do
  home <- getHomeDirectory
  return [home </> ".weaver" </> "weaver.socket"]

data Handshake
  = Hello String
  | Goodbye String
  deriving (Show, Generic)

instance Store Handshake

data Request
  = RunShellCommand Text
  | RunCommand [Text]
  | KillProcess ProcessId
  | SendInput ProcessId [Text]
  deriving (Show, Generic)

instance Store Request

newtype ProcessId = ProcessId { getProcessId :: Int } deriving (Show, Generic)

instance Store ProcessId

data WeaverProcess = WeaverProcess {
    processId     :: Int
  , processName   :: Text
  , processOutput :: BS.ByteString
} deriving (Show, Generic)

instance Store WeaverProcess

data WeaverEvent
  = ProcessLaunched WeaverProcess
  | ProcessOutput ProcessId BS.ByteString
  | ProcessTerminated ProcessId Integer
  deriving (Show, Generic)

instance Store WeaverEvent
