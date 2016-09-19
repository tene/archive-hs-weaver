{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List            as DCL

import           Weaver


main :: IO ()
main = weaverConnect Nothing echoClient

echoClient :: WeaverEventSource -> WeaverRequestSink -> IO ()
echoClient events requests = do
  yield (Hello "client") $$ requests
  (msg :: Maybe WeaverEvent) <- runResourceT $ events $$ DCL.head
  print msg
