{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Conduit
import qualified Data.Conduit.List as DCL

import           Weaver


main :: IO ()
main = weaverConnect Nothing echoClient

echoClient :: WeaverEventSource -> WeaverRequestSink -> IO ()
echoClient events requests = do
  runConduit $ yield (Hello "client") .| requests
  (msg :: Maybe WeaverEvent) <- runConduitRes $ events .| DCL.head
  print msg
