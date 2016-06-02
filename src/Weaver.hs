{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
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

instance Binary Request

newtype ProcessId = ProcessId { getProcessId :: Int } deriving (Show, Generic)
instance Binary ProcessId

data WeaverProcess = WeaverProcess {
    processId     :: Int
  , processName   :: Text
  , processOutput :: BS.ByteString
} deriving (Show, Generic)
instance Binary WeaverProcess

data WeaverEvent
  = ProcessLaunched WeaverProcess
  | ProcessOutput ProcessId BS.ByteString
  | ProcessTerminated ProcessId Integer
  deriving (Show, Generic)
instance Binary WeaverEvent
