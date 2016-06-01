{-# LANGUAGE DeriveGeneric #-}
module Weaver where

import           Data.Binary
import qualified Data.ByteString  as BS
import           Data.Text
import           GHC.Generics     (Generic)
import           GHC.IO.Exception (ExitCode (..))


data Message
  = Hello String
  | Goodbye String
  deriving (Show, Generic)

instance Binary Message

data Request
  = RunShellCommand Text
  | RunCommand [Text]
  | KillProcess ProcessId
  | SendInput ProcessId [Text]
  deriving (Show, Generic)

newtype ProcessId = ProcessId { getProcessId :: Int } deriving (Show, Generic)

data WeaverProcess = WeaverProcess {
    processId     :: Int
  , processName   :: Text
  , processOutput :: BS.ByteString
} deriving (Show, Generic)

data WeaverEvent
  = ProcessLaunched WeaverProcess
  | ProcessOutput ProcessId BS.ByteString
  | ProcessTerminated ProcessId ExitCode
  deriving (Show, Generic)
