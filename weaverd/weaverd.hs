import qualified Data.Binary                   as Binary
import qualified Data.ByteString.Lazy          as BSL
import           Network.Endpoints
import           Network.Transport.Sockets.TCP
import           System.Environment

import           Weaver

main = do
  [listenAddr] <- getArgs
  ep           <- newEndpoint
  let name = Name listenAddr
      resolver = tcpSocketResolver4
  putStrLn $ "Echo server started at " ++ listenAddr
  withTransport (newTCPTransport4 resolver) $ \transport ->
    withEndpoint transport ep $
      withBinding transport ep name $
        echoServer ep

echoServer :: Endpoint -> IO ()
echoServer ep = go
  where
    go :: IO ()
    go = do
      msg <- receiveMessage ep
      let hello = (Binary.decode (BSL.fromStrict msg) :: Weaver.Message)
      case hello of
        Hello name -> sendMessage ep (Name name) $ BSL.toStrict $ Binary.encode $ Goodbye "server"
        _          -> return ()
      go
