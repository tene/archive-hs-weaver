{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Weaver where

import qualified Data.ByteString               as BS
import           Data.Serialize                (Serialize (..))
import qualified Data.Serialize                as Serialize
import           Data.Serialize.Text           ()
import           Data.Text
import           GHC.Generics                  (Generic)
import           GHC.IO.Exception              (ExitCode (..))
import qualified Network.Endpoints             as NE
import qualified Network.RPC                   as RPC
import           Network.Transport.Sockets.TCP

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

data Context = Context {
    contextTransport :: Transport
  , contextEndpoint  :: NE.Endpoint
  , contextName      :: NE.Name
  , contextCallSite  :: RPC.CallSite
}

-- I'm not entirely happy with the RPC api
-- TODO reimplement a very similar API that exposes the name of the caller

withWeaver :: String -> String -> (Context -> IO ()) -> IO ()
withWeaver targetStr clientStr action = do
  ep <- NE.newEndpoint
  let resolver = tcpSocketResolver4
      target   = NE.Name targetStr
      client   = NE.Name clientStr
      callsite = RPC.newCallSite ep client
  withTransport (newTCPTransport4 resolver) $ \transport ->
    withEndpoint transport ep $
      NE.withName ep client $
        withConnection transport ep target $ do
          let ctx = Context transport ep client callsite
          action ctx

weaverCall :: (Serialize.Serialize a, Serialize.Serialize b) =>
     Context -> String -> RPC.Method -> a -> IO (Either String b)
weaverCall ctx target method msg = do
  rv <- RPC.call (contextCallSite ctx) (NE.Name target) method (Serialize.encode msg)
  return $ Serialize.decode rv
