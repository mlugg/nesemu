module Util where

import Data.IORef
import Data.Bits
import Data.Word
import Data.Int

infixl 8 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

mkLE :: Word8 -> Word8 -> Word16
mkLE x y = fromIntegral x .|. fromIntegral y `shiftL` 8

deLE :: Word16 -> (Word8, Word8)
deLE x = (fromIntegral x .&. 0xFF, fromIntegral $ x `shiftR` 8)

setClearBit :: (Bits a) => Int -> Bool -> a -> a
setClearBit b True  x = x `setBit`   b
setClearBit b False x = x `clearBit` b

cvt16 :: Word8 -> Word16
cvt16 = fromIntegral

cvt8 :: Word16 -> Word8
cvt8 = fromIntegral

sign8 :: Word8 -> Int8
sign8 = fromIntegral

cvt32S :: Int8 -> Int32
cvt32S = fromIntegral
