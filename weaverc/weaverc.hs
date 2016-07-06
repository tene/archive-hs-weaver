{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.ByteString
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.Cereal       (conduitGet2, conduitPut, sinkGet,
                                            sourcePut)
import           Data.Conduit.Network.Unix (AppDataUnix, appSink, appSource,
                                            clientSettings, runUnixClient)
import           Data.Serialize
import           System.Environment
import           System.IO                 (stdout)

import           Weaver


main :: IO ()
main = do
  [listenAddr] <- getArgs
  runUnixClient (clientSettings listenAddr) echoClient

sendMessage :: Monad m => Message -> Producer m ByteString
sendMessage msg = sourcePut $ put msg

echoClient :: AppDataUnix -> IO ()
echoClient app = do
  sendMessage (Hello "client") $$ (appSink app)
  msg <- (appSource app) $$ sinkGet (get :: Get Weaver.Message)
  print msg
