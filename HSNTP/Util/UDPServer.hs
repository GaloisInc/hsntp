module HSNTP.Util.UDPServer (UDPServer(..), stdUDPServer, runUDPServer, waitForever) where

import HSNTP.Util.Misc
import HSNTP.Util.UDP

import Control.Concurrent
import Control.Exception
import Control.Monad
import Foreign
import Foreign.Ptr
import Network.Socket
import Prelude hiding(catch)

type Bufi = (Ptr Word8, Int)

data UDPServer a = UDPServer { putFun :: a -> Bufi -> IO Int,
                               getFun :: Bufi -> IO a,
                               workFun:: a -> IO a,
                               excFun :: SomeException -> IO (),
                               timeout:: Time,
                               bufSize:: Int,
                               threads:: Int,
                               port   :: Int
                             }

stdUDPServer :: UDPServer a
stdUDPServer = UDPServer { putFun  = \_ _ -> return 0,
                           getFun  = \_   -> return undefined,
                           workFun = return,
                           excFun  = print,
                           timeout = seconds 300,
                           bufSize = 512,
                           threads = 10,
                           port    = 0
                         }


run :: UDPServer s -> Socket -> Ptr Word8 -> IO ()
run udpc sock ptr = runWithTO' (timeout udpc) work `catch` exc
    where exc   = excFun udpc
          work  = do (len,sa) <- recvBufFrom sock ptr (bufSize udpc)
                     packet   <- (getFun udpc) (ptr,len)
                     reply    <- (workFun udpc) packet
                     rlen     <- (putFun udpc) reply (ptr,bufSize udpc)
                     sendBufTo sock ptr rlen sa
                     return ()


par :: Int -> IO () -> IO [ThreadId]
par n c = replicateM n (forkIO c)

withBufLoop :: UDPServer s -> Socket -> IO b
withBufLoop udpc sock = allocaArray (bufSize udpc) loop
    where loop ptr = run udpc sock ptr >> loop ptr

runUDPServer :: UDPServer s -> IO [ThreadId]
runUDPServer udpc = do sock <- listenUDP (port udpc)
                       par (threads udpc) $ withBufLoop udpc sock

-- Utility for clients
waitForever :: IO ()
waitForever = newEmptyMVar >>= takeMVar >> return ()

