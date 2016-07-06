{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Weaver where

import qualified Data.ByteString      as BS
import           Data.Serialize       (Serialize (..))
import qualified Data.Serialize       as Serialize
import           Data.Serialize.Text  ()
import           Data.Text
import           GHC.Generics         (Generic)
import           System.Posix.Process (getProcessID)

data Message
  = Hello String
  | Goodbye String
  deriving (Show, Generic)

instance Serialize Message

data Request
  = RunShellCommand Text
  | RunCommand [Text]
  | KillProcess ProcessId
  | SendInput ProcessId [Text]
  deriving (Show, Generic)

instance Serialize Request

newtype ProcessId = ProcessId { getProcessId :: Int } deriving (Show, Generic)
instance Serialize ProcessId

data WeaverProcess = WeaverProcess {
    processId     :: Int
  , processName   :: Text
  , processOutput :: BS.ByteString
} deriving (Show, Generic)
instance Serialize WeaverProcess

data WeaverEvent
  = ProcessLaunched WeaverProcess
  | ProcessOutput ProcessId BS.ByteString
  | ProcessTerminated ProcessId Integer
  deriving (Show, Generic)
instance Serialize WeaverEvent
