import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Serialize                as Serialize
import           Network.Endpoints as NE
import           Network.RPC as RPC
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
        let cs = RPC.newCallSite ep name
            ctx = Context transport ep name cs
        in echoServer ctx

hearWeaver ctx method = do
  (msg, reply) <- RPC.hear (contextEndpoint ctx) (contextName ctx) method
  return ((Serialize.decode msg), reply)

echoServer :: Context -> IO ()
echoServer ctx = go
  where
    go :: IO ()
    go = do
      (msg, reply) <- hearWeaver ctx "echo"
      case msg of
        Right (Hello name) -> reply $ Serialize.encode $ Goodbye "server"
        _          -> return ()
      go
