import           Data.Conduit
import           Data.Conduit.Cereal       (conduitGet2, conduitPut)
import           Data.Conduit.Network.Unix (AppDataUnix, ServerSettingsUnix,
                                            appSink, appSource, runUnixServer,
                                            serverSettings)
import           Data.Serialize
import           System.Environment

import           Weaver

main :: IO ()
main = do
  [listenAddr] <- getArgs
  runUnixServer (serverSettings listenAddr) echoServer

hello2goodbye = do
  msg <- await
  case msg of
    Just (Hello name) -> do
      yield $ Goodbye name
      hello2goodbye
    _ -> return ()

echoServer :: AppDataUnix -> IO ()
echoServer app = (appSource app) =$= (conduitGet2 (get :: Get Weaver.Message)) =$= hello2goodbye =$= (conduitPut (put :: Putter Weaver.Message)) $$ (appSink app)
