module RomParse where

-- Parses iNES ROMs (ugh)

import Data.Binary.Strict.Get
import Data.Word
import Data.Bits
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as V

expect :: Word8 -> Get ()
expect x =
  getWord8 >>= \y ->
  if y == x
  then pure ()
  else fail $ "Expected byte " <> show x <> " but got " <> show y

data Mirroring = MirHorizontal | MirVertical | Mir4Screen
  deriving (Show)

data TVType = PAL | NTSC
  deriving (Show)

data INESHdr = INESHdr
  { inesPrgRomSize    :: Word64
  , inesChrRomSize    :: Word64
  , inesPrgRamSize    :: Word64
  , inesMapper        :: Word8
  , inesMirroring     :: Mirroring
  , inesHasBatteryPrg :: Bool
  , inesHasTrainer    :: Bool
  , inesVsGame        :: Bool
  , inesPc10Data      :: Bool
  , inesTvType        :: TVType
  } deriving (Show)

data INESRom = INESRom
  { romHdr    :: INESHdr
  , romPrgRom :: BS.ByteString
  , romChrRom :: BS.ByteString
  } deriving (Show)

parseHeader :: Get INESHdr
parseHeader = do
  expect 0x4E  -- 'N'
  expect 0x45  -- 'E'
  expect 0x53  -- 'S'
  expect 0x1A
  prgRomSize <- getWord8
  chrRomSize <- getWord8
  f6 <- getWord8
  f7 <- getWord8
  f8 <- getWord8
  f9 <- getWord8
  f10 <- getWord8
  skip 5 -- unused padding bytes

  when ((f7 `shiftR` 2 .&. 3) == 2) (fail $ "NES 2.0 ROM unsupported")

  pure $ INESHdr
    { inesPrgRomSize = fromIntegral prgRomSize * 16384
    , inesChrRomSize = fromIntegral chrRomSize * 8192
    , inesPrgRamSize =
        if f8 == 0
        then 8192
        else 8192 * fromIntegral f8
    , inesMapper =
        (f7 .&. 0xF0) .|. f6 `shiftR` 4
    , inesMirroring =
        if testBit f6 3
        then Mir4Screen
        else if testBit f6 0
        then MirHorizontal
        else MirVertical
    , inesHasBatteryPrg = testBit f6 1
    , inesHasTrainer = testBit f6 2
    , inesVsGame = testBit f7 0
    , inesPc10Data = testBit f7 1
    , inesTvType =
        if testBit f9 0
        then PAL
        else NTSC }

parseRom :: Get INESRom
parseRom = do
  hdr <- parseHeader
  prgRom <- getByteString (fromIntegral $ inesPrgRomSize hdr)
  chrRom <- getByteString (fromIntegral $ inesChrRomSize hdr)
  if inesPc10Data hdr
  then skip $ 8192 + 32
  else pure ()

  end <- isEmpty

  if not end
  then fail "Parsed full ROM but there was still data left"
  else pure $ INESRom hdr prgRom chrRom

mkMem :: INESRom -> IO (V.IOVector Word8)
mkMem r = do
  mem <- V.new 65536
  zip [0x8000..] (BS.unpack $ romPrgRom r) `forM_` (\(a,x) -> V.write mem a x)
  zip [0xC000..] (BS.unpack $ romPrgRom r) `forM_` (\(a,x) -> V.write mem a x)
  pure mem
