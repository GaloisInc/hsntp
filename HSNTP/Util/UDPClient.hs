module HSNTP.Util.UDPClient (UDPClient(..), stdUDPClient,
                       runUDPClient, seconds, sockAddr)
    where

import Control.Exception
import Control.Monad
import Foreign
import Foreign.Ptr
import Network.Socket hiding (sClose)
import Prelude hiding(catch)

import HSNTP.Util.Misc
import HSNTP.Util.UDP

type Bufi = (Ptr Word8, Int)

data UDPClient a s = UDPClient { putFun :: Bufi -> IO (Int,s),
                                 getFun :: Bufi -> IO a,
                                 valFun :: s -> a -> a,
                                 retries:: Int,
                                 timeout:: Time,
                                 bufSize:: Int,
                                 destSA :: SockAddr
                               }

stdUDPClient :: UDPClient a1 a
stdUDPClient = UDPClient { putFun  = \_ -> return (0,undefined),
                           getFun  = \_ -> return undefined,
                           valFun  = const id,
                           retries = 1,
                           timeout = seconds 10,
                           bufSize = 512,
                           destSA  = undefined
                         }

decRetries :: UDPClient a s -> UDPClient a s
decRetries udpc = udpc { retries = retries udpc - 1 }

runUDPClient :: UDPClient a s -> IO a
runUDPClient udpc = runWithTO' (timeout udpc) (runUDPClient' udpc) `catch` exc udpc
    where
      exc :: UDPClient a s -> SomeException -> IO a
      exc udpc e = if retries udpc > 0
                     then runUDPClient $ decRetries udpc
                     else throw e

runUDPClient' :: UDPClient a s -> IO a
runUDPClient' udpc = allocaArray (bufSize udpc) in1
    where in1 ptr = bracket newSock sClose (in2 ptr)
          in2 ptr s = in3 ptr s
          in3 ptr s = do (l,st)   <- putFun udpc $ (ptr,bufSize udpc)
                         sendBufTo s ptr l (destSA udpc)
                         (rl,rsa) <- recvBufFrom s ptr (bufSize udpc)
                         when (rsa /= destSA udpc) $ fail "reply from wrong SockAddr"
                         val <- (getFun udpc) (ptr,rl)
                         return $ (valFun udpc) st val

