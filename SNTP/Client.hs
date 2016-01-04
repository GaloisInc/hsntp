{-# LANGUAGE ForeignFunctionInterface #-}
module SNTP.Client (query, client, setTime) where

import SNTP.SNTP
import HSNTP.Util.UDPClient

import Control.Monad.Error
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString)
import Network.Socket (SockAddr)
import Prelude hiding(catch)

type HostName = String

query :: HostName -> IO Packet
query host = do sa <- sockAddr host 123
                runUDPClient $ client sa

-- client :: Network.Socket.SockAddr -> UDPClient Packet SNTP.SNTP.TimeStamp
client sa = stdUDPClient { putFun = putPacket $ emptyPacket { word0 = liVerMode 0 1 3},
                           getFun = parsePacket,
                           valFun = \t p -> case origTS p == t of ; True -> p,
                           destSA = sa
                         }

foreign import ccall "sntp_strerr" sntp_strerr :: IO CString
foreign import ccall "sntp_set_time_large" sntp_set_time_large :: Word32 -> Word32 -> IO Int
foreign import ccall "sntp_set_time_small" sntp_set_time_small :: Word32 -> Word32 -> IO Int

setTime :: Packet -> IO ()
setTime p = do let tfun = if tdiff p < 0.5 then sntp_set_time_small else sntp_set_time_large
               uncurry tfun (dToSecMSec $ tdiff p) >>= errs

errs :: (Num t, Eq t) => t -> IO ()
errs 0 = return ()
errs _ = sntp_strerr >>= peekCString >>= \s -> fail ("setting time: "++s)

