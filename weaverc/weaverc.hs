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

sendHandshake :: Monad m => Handshake -> Producer m ByteString
sendHandshake msg = yield (Message msg) =$= conduitEncode

echoClient :: AppDataUnix -> IO ()
echoClient app = do
  sendHandshake (Hello "client") $$ (appSink app)
  (msg :: Maybe Handshake) <- runResourceT $ runConduit $ (appSource app)
    =$= conduitDecode Nothing
    =$= DCL.map fromMessage
    =$= DCL.head
  print msg
