{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.Trans.Resource
import           Data.ByteString
import           Data.Conduit
import qualified Data.Conduit.List            as DCL
import           Data.Conduit.Network.Unix    (AppDataUnix, appSink, appSource,
                                               clientSettings, runUnixClient)
import           Data.Store.Streaming         (Message (..), conduitDecode,
                                               conduitEncode, fromMessage)
import           System.Environment
import           System.IO                    (stdout)

import           Weaver


main :: IO ()
main = weaverConnect echoClient

echoClient :: WeaverEventSource -> WeaverRequestSink -> IO ()
echoClient events requests = do
  yield (Hello "client") $$ requests
  (msg :: Maybe WeaverEvent) <- runResourceT $ events $$ DCL.head
  print msg
