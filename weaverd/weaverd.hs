import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Network.Unix    (AppDataUnix, appSink, appSource,
                                               runUnixServer, serverSettings)
import           Data.Store.Streaming         (Message (..), conduitDecode,
                                               conduitEncode)
import           Safe
import           System.Environment

import           Weaver

main :: IO ()
main = do
  args <- getArgs
  let name = defaultSocketName $ headMay args
  listenAddr <- getWeaverSocketPath name
  -- TODO check for socket liveness before binding
  runUnixServer (serverSettings listenAddr) (wrapWeaverServer hello2goodbye)

type WeaverServer = Conduit (Message WeaverRequest) (ResourceT IO) (Message WeaverEvent)

hello2goodbye :: WeaverServer
hello2goodbye = awaitForever respond
  where
    respond (Message (Hello name)) = yield $ Message $ Goodbye name
    respond _ = return ()

wrapWeaverServer :: WeaverServer -> AppDataUnix -> IO ()
wrapWeaverServer server app = runResourceT $ runConduit $ (appSource app)
  =$= conduitDecode Nothing
  =$= server
  =$= conduitEncode
  =$= (appSink app)
