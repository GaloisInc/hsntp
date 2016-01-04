{-# LANGUAGE ForeignFunctionInterface #-}
module SNTP.Client (query, queryLiVerMode, client, setTime) where

import SNTP.SNTP
import HSNTP.Util.UDPClient

import Control.Monad.Error
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, peekCString)
import Network.Socket (SockAddr)
import Prelude hiding(catch)

type HostName = String


-- | Query the given host with a specified Leap Interval, Version, Mode, and Precision
queryLiVerMode :: Word8 -> Word8 -> Word8 -> Word8 -> Word32 -> Word32 -> Word8 -> HostName -> IO Packet
queryLiVerMode li ver mode precision rDelay rDisp ppoll host = do
  sa <- sockAddr host 123
  runUDPClient $ clientDetails sa
  where
    clientDetails sa = stdUDPClient
                       { putFun = putPacket $ packet
                       , getFun = parsePacket
                       , valFun = \t p -> case origTS p == t of ; True -> p
                       , destSA = sa
                       }

    packet = emptyPacket { word0 = liVerMode li ver mode
                         , precision = precision
                         , rootDelay = rDelay
                         , rootDisp = rDisp
                         , poll = ppoll
                         }

query :: HostName -> IO Packet
query host = do sa <- sockAddr host 123
                runUDPClient $ client sa

-- client :: Network.Socket.SockAddr -> UDPClient Packet SNTP.SNTP.TimeStamp
client sa = stdUDPClient { putFun = putPacket $ emptyPacket { word0 = liVerMode 0 4 3},
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

