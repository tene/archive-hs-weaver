import           Control.Concurrent
import           Control.Exception
import           Data.Binary
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  (fromChunks)
import           Data.Map
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.Environment

import           Weaver

main = do
  [host, port] <- getArgs
  serverDone      <- newEmptyMVar
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport
  forkIO $ echoServer endpoint serverDone
  putStrLn $ "Echo server started at " ++ show (address endpoint)
  readMVar serverDone `onCtrlC` closeTransport transport

echoServer :: EndPoint -> MVar () -> IO ()
echoServer endpoint serverDone = go empty
  where
    go :: Map ConnectionId (MVar Connection) -> IO ()
    go cs = do
      event <- receive endpoint
      case event of
        ConnectionOpened cid rel addr -> do
          connMVar <- newEmptyMVar
          forkIO $ do
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn
          go (insert cid connMVar cs)
        Received cid payload -> do
          forkIO $ do
            conn <- readMVar (cs ! cid)
            send conn [ pack $ show $ ( decode $ fromChunks payload :: Message ) ]
            return ()
          go cs
        ConnectionClosed cid -> do
          forkIO $ do
            conn <- readMVar (cs ! cid)
            close conn
          go (delete cid cs)
        EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing
