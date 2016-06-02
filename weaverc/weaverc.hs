{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Binary                   as Binary
import qualified Data.ByteString.Lazy          as BSL
import           Network.Endpoints
import           Network.Transport.Sockets.TCP
import           System.Environment

import           Weaver


main = do
  [targetStr, clientStr] <- getArgs
  let resolver = tcpSocketResolver4
      target   = Name targetStr
      client   = Name clientStr
  ep <- newEndpoint
  withTransport (newTCPTransport4 resolver) $ \transport ->
    withEndpoint transport ep $
      withName ep client $
        withConnection transport ep target $ do
          sendMessage ep target $ BSL.toStrict $ Binary.encode $ Hello clientStr
          msg <- receiveMessage ep
          print (Binary.decode (BSL.fromStrict msg) :: Weaver.Message)
  return ()
