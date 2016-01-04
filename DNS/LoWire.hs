{-# LANGUAGE ForeignFunctionInterface #-}
module DNS.LoWire (getW16, getW32, getW8Lst, getW16Lst, getName,
                   putW16, putW32, putW8Lst, putW16Lst, putName,
                   atEnd, getW64, putW64
                  ) where

import DNS.Type
import Control.Monad.Error (throwError)
import Control.Monad.State
import Foreign hiding (newArray)
import Data.Array.IO
import Data.Array.Unboxed

foreign import ccall unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32
foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import ccall unsafe "htons" htons :: Word16 -> Word16


advanceBufi :: Bufi -> Int -> Bufi
advanceBufi (p,l) n = (advancePtr p n,l-n)

prec :: Bool -> String -> MayIO ()
prec b s = when b (throwError s)


{-# INLINE ioToMayIOSt #-}
ioToMayIOSt :: IO a -> MayIOSt s a
ioToMayIOSt = lift . liftIO


advanceBufSt :: Int -> MayIOSt WState ()
advanceBufSt iv = modify (\(lnc,pos,bp) -> (lnc,advanceBufi pos iv,bp))

pState :: String -> MayIOSt WState ()
pState = const $ return ()
--pState s = get >>= \x -> ioToMayIOSt $ putStrLn (s++" "++show x)

lookBuf :: Int -> MayIOSt WState Bufi
lookBuf iv = do (_,x@(_,len),_) <- get
                lift $ prec (len < iv) "buffer underflow"
                return x


getW8Lst :: Int -> MayIOSt WState [Word8]
getW8Lst n = do (p,_) <- lookBuf n
                advanceBufSt n
                ioToMayIOSt $ peekArray n (castPtr p)

getW16Lst :: Int -> MayIOSt WState [Word16]
getW16Lst n = do (p,_) <- lookBuf (n * 2)
                 advanceBufSt (n * 2)
                 lst <- ioToMayIOSt $ peekArray n (castPtr p)
                 return $ map ntohs lst

getName :: MayIOSt WState Name
getName = do pState "getName"
             nl <- getNamE 10
             return $ listArray (0,length nl - 1) nl

getNameA :: Ptr Word8 -> Int -> Int -> MayIOSt WState (IOUArray Int Word8)
getNameA _ _ 0 = lift $ throwError "getNameA loop"
getNameA b o n = do (new,_) <- lookBuf 1
                    let len = minusPtr new b
                    ch <- ioToMayIOSt (peek new >>= return . fromEnum)
                    case ch of
                     0          -> do arr <- ioToMayIOSt $ newArray_ (0,o+len)
                                      lst <- ioToMayIOSt $ peekArray len b
                                      ioToMayIOSt $ dirtyW arr o lst
                                      advanceBufSt 1
                                      return arr
                     x | x < 64 -> advanceBufSt (x+1) >> getNameA b o n
                       | True   -> do nextUM <- getW16
                                      save@(lc,(p,l),bp) <- get
                                      let x = nextUM .&. 16383
                                          new = advancePtr bp (fromEnum x)
                                          diff= minusPtr new p
                                      put (lc,(new,l-diff), bp)
                                      arr <- getNameA new (o+len) (n-1)
                                      lst <- ioToMayIOSt $ peekArray len b
                                      ioToMayIOSt $ dirtyW arr o lst
                                      put save
                                      advanceBufSt 1
                                      return arr

dirtyW :: IOUArray Int Word8 -> Int -> [Word8] -> IO ()
dirtyW arr off lst = foldM_ (\i v -> writeArray arr i v >> return (i+1)) off lst


getNamE :: Int -> MayIOSt WState [Word8]
getNamE 0 = lift $ throwError "getNamE loop"
getNamE n = loop
    where mut x (ln,(p,l),bp) = let new = advancePtr bp (fromEnum x)
                                    diff= minusPtr new p
                                    in (ln,(new,l-diff),bp)
          loop = do (ptr,len) <- lookBuf 1
                    x <- ioToMayIOSt $ (peek ptr >>= return . fromIntegral)
                    case x of
                     0          -> do advanceBufSt 1
                                      return [0]
                     x | x < 64 -> do lift $ prec (len - x <= 0) "getName: tried buffer overflow"
                                      seg   <- ioToMayIOSt $ peekArray x (advancePtr ptr 1)
                                      advanceBufSt (x+1)
                                      rest  <- getNamE n
                                      return $ fromIntegral x : seg++rest
                       | True   -> do nextUM <- getW16
                                      let x = nextUM .&. 16383
                                      pState (" name comp, x = "++show x)
                                      save <- get
                                      modify (mut x)
                                      pState (" mutated")
                                      rest <- getNamE (n-1)
                                      put save
                                      return rest




getW32 :: MayIOSt WState Word32
getW32 = do (ptr,len) <- lookBuf 4
            w32 <- ioToMayIOSt $ peek (castPtr ptr)
--          pState (" getW32: "++show w32++" -> "++show (htonl w32))
            advanceBufSt 4
            return (ntohl w32)

getW16 :: MayIOSt WState Word16
getW16 = do (ptr,_) <- lookBuf 2
            w16 <- ioToMayIOSt $ peek (castPtr ptr)
            advanceBufSt 2
            return (ntohs w16)


getW64 :: MayIOSt WState Word64
getW64 = do w1 <- getW32
            w2 <- getW32
            return $ (fromIntegral w1 `shiftL` 32) + fromIntegral w2

-- | Is the get at the end of the buffer (for optional fields)
atEnd :: MayIOSt WState Bool
atEnd = get >>= \(_,(_,l),_) -> return $ l == 0

-- * write routines *unsafe*

putW16 :: Word16 -> PSt ()
putW16 w = get >>= (\p -> liftIO (poke (castPtr p) (htons w)) >> put (advancePtr p 2))

putW32 :: Word32 -> PSt ()
putW32 w = get >>= (\p -> liftIO (poke (castPtr p) (htonl w)) >> put (advancePtr p 4))

putW64 :: Word64 -> PSt ()
putW64 w = do putW32 $ fromIntegral $ w `shiftR` 32
              putW32 $ fromIntegral $ w .&. 0o37777777777

putW16Lst :: [Word16] -> PSt ()
putW16Lst lst = do p <- get
                   liftIO $ pokeArray (castPtr p) (map htons lst)
                   put $ advancePtr p (2 * length lst)

putW8Lst :: [Word8] -> PSt ()
putW8Lst lst = do p <- get
                  liftIO $ pokeArray p lst
                  put $ advancePtr p (length lst)

putName :: Name -> PSt ()
putName = putW8Lst . elems

-- Length of various components

-- i2w = fromIntegral

w16p :: Ptr a -> Ptr Word16
w16p = castPtr

