{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Network.Unix    (AppDataUnix, appSink, appSource,
                                               runUnixServer, serverSettings)
import           Data.IORef                   (IORef, atomicModifyIORef,
                                               newIORef)
import           Data.Store.Streaming         (Message (..), conduitDecode,
                                               conduitEncode)
import           Safe
import           System.Environment
import           System.IO.Unsafe             (unsafePerformIO)

import           Weaver

main :: IO ()
main = do
  args <- getArgs
  let name = defaultSocketName $ headMay args
  listenAddr <- getWeaverSocketPath name
  -- TODO check for socket liveness before binding
  runUnixServer (serverSettings listenAddr) (wrapWeaverServer hello2goodbye)

type WeaverServer = Conduit (Message WeaverRequest) (ResourceT IO) (Message WeaverEvent)

-- XXX TODO work out a sane way to track unique processes over daemon lifetime
ughPidCount :: IORef Int
{-# NOINLINE ughPidCount #-}
ughPidCount = unsafePerformIO (newIORef 0)

fakeRunShellCommand :: String -> WeaverServer
fakeRunShellCommand cmd = do
  pid <- liftIO $ atomicModifyIORef ughPidCount (\x -> (x+1,ProcessId x))
  yield $ Message $ ProcessLaunched $ WeaverProcess pid cmd ""
  yield $ Message $ ProcessOutput pid "blah\n"
  yield $ Message $ ProcessTerminated pid 0

hello2goodbye :: WeaverServer
hello2goodbye = awaitForever respond
  where
    respond (Message (Hello name)) = yield $ Message $ Goodbye name
    respond (Message (RunShellCommand cmd)) = fakeRunShellCommand cmd
    respond _ = return ()

wrapWeaverServer :: WeaverServer -> AppDataUnix -> IO ()
wrapWeaverServer server app = runConduitRes $ (appSource app)
  .| conduitDecode Nothing
  .| server
  .| conduitEncode
  .| (appSink app)
