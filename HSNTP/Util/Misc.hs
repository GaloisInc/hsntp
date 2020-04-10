module HSNTP.Util.Misc (udpQuery, runWithTO', runWithTO, Time, seconds, MayIO) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import Foreign
import Foreign.Ptr
import Network.Socket hiding (sClose)
import Prelude hiding (catch)

import HSNTP.Util.UDP

withUDPBuf :: Int -> ((Socket, BPtr) -> IO a) -> IO a
withUDPBuf n = bracket start end
    where start      = liftM2 (,) newSock (mallocArray n)
          end  (s,p) = sClose s >> free p


type MayIO = ErrorT String IO
type Port = Int
type BPtr = Ptr Word8
type Bufi = (Ptr Word8,Int)

udpQuery :: String -> Port -> Int -> Time -> (Bufi -> MayIO Int) -> (Bufi -> MayIO a) -> MayIO a
udpQuery host port blen time put get = liftIO base >>= reith
    where base = runWithTO time (withUDPBuf blen (\x -> runErrorT (work x)))
          reith :: Either String a -> MayIO a
          reith (Left e) = throwError e
          reith (Right v)= return v
          work (sock,ptr)= do len <- put (ptr,blen)
                              sa  <- liftIO $ sockAddr host port
                              liftIO $ sendBufTo sock ptr len sa
                              (len',sa') <- liftIO $ recvBufFrom sock ptr blen
                              when (sa /= sa') $ throwError "Reply from wrong sockAddr"
                              get (ptr,len')


runWithTO :: Time -> IO (Either String b) -> IO (Either String b)
runWithTO (T t) co = do mv <- newEmptyMVar
                        c1 <- forkIO $ (co >>= putMVar mv) `catch` exc mv
                        c2 <- forkIO $ threadDelay t >> putMVar mv (Left "Timeout")
                        val<- takeMVar mv
                        killThread c1; killThread c2
                        return val
    where
      exc :: MVar (Either String b) -> SomeException -> IO ()
      exc mv = \e -> putMVar mv (Left (show e))

runWithTO' :: Time -> IO t -> IO t
runWithTO' (T t) co = do mt <- myThreadId
                         c  <- forkIO $ threadDelay t >> throwTo mt (AssertionFailed "timeout")
                         res<- co
                         killThread c
                         return res


newtype Time = T Int
seconds :: Int -> Time
seconds n = T $ n * 1000 * 1000
