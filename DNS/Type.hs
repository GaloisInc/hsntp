{-# -fglasgow-exts #-}
module DNS.Type (question, rquestion,
		 putLine, enc, Bufi, Data,
		 Question(Q), MayIO,
		 RR(RR), RClass, RType,
		 Name, satisfies, Zone(..), MayIOSt,
		 Packet(..), converge, WState, PSt,
		 errorPacket, emptyPacket, hashQuestion
		) where

import Data.Char
import Data.Int (Int32)
import Data.Bits(shiftR)
import Data.Word
import Foreign.Ptr
import Control.Monad.Error
import Control.Monad.State
import Data.Array.Unboxed

type MayIO = ErrorT String IO
type MayIOSt s = StateT s MayIO
type WState    = ((), Bufi, Ptr Word8)
type PSt = StateT (Ptr Word8) IO


putLine :: String -> MayIO ()
putLine s = liftIO $ putStrLn s

-- Questions

type Data = UArray Int Word8

data Question = Q !Name !QType !QClass

instance Eq Question where
    (==) (Q an at ac) (Q bn bt _) = an == bn && at `teq` bt && ac `teq` bt

teq :: Word16 -> Word16 -> Bool
teq 255 _ = True
teq _ 255 = True
teq x y   = x == y

satisfies :: Question -> RR -> Bool
satisfies (Q n t c) (RR rn rt rc _ _) = n == rn && (rt == 5 || t == 255 || t == rt) && c `teq` rc

type QType = Word16
type QClass= Word16
type QId   = Word16
type Bufi  = (Ptr Word8, Int)

question :: String -> QType -> QClass -> Question
question a b c = Q (enc a) b c
rquestion a b c = Q a b c

hashQuestion :: Question -> Int32
hashQuestion (Q n t _) = fromIntegral $ fromEnum t + foldl fun 0 lst
    where lst = take 10 $ drop 4 $ elems n
	  fun a e = fromEnum e + (a `shiftR` 7)

instance Show Question where
    show (Q qs qt qc) = "Question: '"++qd qs++"' type="++show qt++" class="++show qc

-- FIXME add safety checks for chunk < 64 and total < 256
enc :: String -> UArray Int Word8
enc s = let lst = enc' s in listArray (0,length lst - 1) lst

enc' s = concatMap (\p -> fromIntegral (length p) : map (fromIntegral . ord) p) $ reverse splitted
    where ein ("",a) '.' = ("",a)
          ein (c,a)  '.' = ("",reverse c : a)
          ein (c,a) ch   = (ch:c,a)
	  norm l@("":_)  = l
          norm lst       = "":lst
	  splitted       = let (t,r) = foldl ein ("",[]) s in norm $ reverse t : r

qd = qd' . elems

qd' :: [Word8] -> String
qd' [] = ""
qd' (n:ns) = let h = map (chr . fromEnum) $ take (fromEnum n) ns
		 t = drop (fromEnum n) ns
		 in if length t <= 1 then h else h ++ '.':qd' t


-- RRs

type RType = Word16
type RClass= Word16
data RR = RR Name RType RClass Word32 Data

instance Show RR where
    show (RR n t c ts d) = unwords ["RR",qd n,show t,show c,show ts,show d]


-- Zones

data Zone = Zone Name [RR] deriving(Show)
type Name = UArray Int Word8


-- Packets

data Packet = Packet { idPQ :: !Word16,
		       hePQ :: !Word16,
		       qsPQ :: ![Question],
		       rsPQ :: ![RR],
		       nsPQ :: ![RR],
		       asPQ :: ![RR]
		     } deriving(Show)


emptyPacket = Packet 0 0 [] [] [] []
errorPacket = emptyPacket { hePQ = 33155 }

related :: Question -> RR -> RR -> Bool
related (Q _ qt qc) (RR _ 5 _ _ ca) (RR rn rt rc _ _) = elems ca == elems rn && qt `teq` rt && qc `teq` rc
related _ _ _ = False


converge :: Word16 -> Question -> Packet -> Packet
converge id q p = let rrs = rsPQ p ++ asPQ p
		      sat = filter (satisfies q) rrs
--		      rel = filter (\rr -> any (\i -> related q i rr) sat) rrs
		      in Packet id (hePQ p) [q] rrs [] []

