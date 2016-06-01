import           Control.Monad
import           Data.Binary
import           Data.ByteString.Char8
import           Data.ByteString.Lazy  (toChunks)
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.Environment

import           Weaver

main = do
  [host, port, serverAddr] <- getArgs
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint <- newEndPoint transport
  let addr = EndPointAddress $ pack serverAddr
  Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
  send conn $ toChunks $ encode $ Hello "lol"
  close conn
  replicateM_ 3 $ receive endpoint >>= print
  closeTransport transport
