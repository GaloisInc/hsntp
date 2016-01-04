{- Simple Network Time Protocoll - RFC 1769
   note RFC 2030
-}
module SNTP.SNTP (Packet(..), putPacket, parsePacket,
		  emptyPacket, tsToClockTime, getCurrentTimeStamp,
		  liVerMode, dToSecMSec, tsToD, delay, tdiff, nilTS
		 ) where

import DNS.LoWire

import Foreign.Ptr
import Data.Char
import Data.Word
import Data.Bits
import Control.Monad.Error
import Control.Monad.State
import System.Time
import System.Random

type MayIO = ErrorT String IO

data Packet = Packet { word0     :: !Word8,
		       stratum   :: !Word8,
		       poll      :: !Word8,
		       precision :: !Word8,
		       rootDelay :: !Word32,
		       rootDisp  :: !Word32,
		       refId     :: String,
		       refTS     :: !TimeStamp,
		       origTS    :: !TimeStamp,
		       recvTS    :: !TimeStamp,
		       transTS   :: !TimeStamp,
		       auth      :: !Auth,
		       received  :: !TimeStamp
		     }


emptyPacket = Packet 0 0 0 0 0 0 "" nilTS nilTS nilTS nilTS Nothing nilTS

packetLength p = maybe 48 (const 64) (auth p)

liVerMode :: Word8 -> Word8 -> Word8 -> Word8
liVerMode li vn mode = (li `shiftL` 6) + (vn `shiftL` 3) + mode

-- TimeStamps
newtype TimeStamp = TS Word64 deriving(Eq)
getTS = do { v <- getW64; return (TS v) }
putTS (TS v) = putW64 v
instance Show TimeStamp where show = show . tsToClockTime
nilTS = TS 0

getRef = do lst <- getW8Lst 4
	    return $ map (chr.fromEnum) $ takeWhile (/= 0) lst
putRef s = putW8Lst $ map (toEnum.ord) $ take 4 (s ++ repeat '\000')

type Auth      = Maybe (Word32,Word32,Word32)
getAuth = do e <- atEnd
	     if e
	      then return Nothing
	      else do a <- getW32; b <- getW32; c <- getW32
		      return $ Just (a,b,c)

putAuth Nothing        = return ()
putAuth (Just (a,b,c)) = putW32 a >> putW32 b >> putW32 c


-- Packets

parsePacket :: (Ptr Word8, Int) -> IO Packet
parsePacket x = runErrorT (evalStateT work ((),x,fst x)) >>= either fail return
    where work = do cts <- liftIO getCurrentTimeStamp
		    [w0,w1,w2,w3] <- getW8Lst 4
		    d0 <- getW32; d1 <- getW32; d2 <- getRef
		    t0 <- getTS;  t1 <- getTS;
		    t2 <- getTS;  t3 <- getTS;
		    au <- getAuth
		    return $ Packet w0 w1 w2 w3 d0 d1 d2 t0 t1 t2 t3 au cts

putPacket ::  Packet -> (Ptr Word8, Int) -> IO (Int,TimeStamp)
putPacket pa (p,l) =
    do when (packetLength pa > l) $ fail "buffer too short"
       ts <- getCurrentTimeStamp
       evalStateT (wire $ pa { transTS = ts }) p
       return (packetLength pa,ts)
wire (Packet b0 b1 b2 b3 w0 w1 w2 ts0 ts1 ts2 ts3 as _) =
    do putW8Lst [b0,b1,b2,b3]
       putW32 w0 >> putW32 w1 >> putRef w2
       putTS ts0 >> putTS ts1 >> putTS ts2 >> putTS ts3
       putAuth as

--  On 1 January 1970 when Unix life began, NTP time was 2208988800
tsToClockTime (TS s) = TOD (fromIntegral (s `shiftR` 32) - 2208988800) 0


-- 1000000000000 psec = 1 sec = 2**32 fract
-- => fract = (1000000000000 / 2**32) * psec
getCurrentTimeStamp :: IO TimeStamp
getCurrentTimeStamp = do iv <- randomIO :: IO Int
			 (TOD sec psec) <- liftIO $ getClockTime
			 let ntp_sec = (2208988800 + fromIntegral sec) `shiftL` 32
			     res     = 0
			     ntp_fra = fromIntegral iv -- round res `xor` fromIntegral (0o3777777 .&. iv)
			 return $ TS $ ntp_sec + ntp_fra

-- Arithmetic FIXME

{-
      Timestamp Name          ID   When Generated
      ------------------------------------------------------------
      Originate Timestamp     T1   time request sent by client
      Receive Timestamp       T2   time request received by server
      Transmit Timestamp      T3   time reply sent by server
      Destination Timestamp   T4   time reply received by client

   The roundtrip delay d and local clock offset t are defined as

      d = (T4 - T1) - (T2 - T3)     t = ((T2 - T1) + (T3 - T4)) / 2.
-}

tsToD :: TimeStamp -> Double
tsToD (TS x) = fromIntegral x / 2**32
-- fromIntegral (x `shiftR` 32) + (fromIntegral (x .&. 0o37777777777) / 2**32)

toD = tsToD

dToSecMSec :: Double -> (Word32,Word32)
dToSecMSec d = let (n,f) = properFraction d in (n, truncate (1000*f))

toI :: TimeStamp -> Integer
toI (TS t) = fromIntegral t

delay packet = (dest - orig) - (recv - trans)
    where orig = toD $ origTS  packet
	  recv = toD $ recvTS  packet
	  trans= toD $ transTS packet
	  dest = toD $ received packet

tdiff packet = ((recv - orig) + (trans - dest)) / 2
    where orig = toD $ origTS  packet
	  recv = toD $ recvTS  packet
	  trans= toD $ transTS packet
	  dest = toD $ received packet
