import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Network.Unix    (AppDataUnix, ServerSettingsUnix,
                                               appSink, appSource,
                                               runUnixServer, serverSettings)
import           Data.Store.Streaming         (Message (..), conduitDecode,
                                               conduitEncode, fromMessage)
import           Safe
import           System.Environment

import           Weaver

main :: IO ()
main = do
  args <- getArgs
  let name = defaultSocketName $ headMay args
  listenAddr <- getWeaverSocketPath name
  runUnixServer (serverSettings listenAddr) echoServer

hello2goodbye :: Monad m => Conduit (Message WeaverRequest) m (Message WeaverEvent)
hello2goodbye = awaitForever respond
  where
    respond (Message (Hello name)) = yield $ Message $ Goodbye name
    respond _ = return ()

echoServer :: AppDataUnix -> IO ()
echoServer app = runResourceT $ runConduit $ (appSource app)
  =$= conduitDecode Nothing
  =$= hello2goodbye
  =$= conduitEncode
  =$= (appSink app)
