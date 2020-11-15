{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}

module Common where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Text.Printf
import Prelude hiding (log, read)
import Util
import Control.Monad.Reader
import Data.Vector.Unboxed.Mutable
import Data.Word
import Colog
import Data.IORef
import Data.Bits
import Data.Text
import Data.Functor
import Control.Monad

data DataLoc
  = RA
  | RX
  | RY
  | RS
  | Mem Word16

readVal :: DataLoc -> NES Word8
readVal l = case l of
  RA -> readCPU cpuRegA
  RX -> readCPU cpuRegX
  RY -> readCPU cpuRegY
  RS -> readCPU cpuRegS
  Mem addr -> readMem8 addr

writeVal :: DataLoc -> Word8 -> NES ()
writeVal l x = case l of
  RA -> writeCPU cpuRegA x
  RX -> writeCPU cpuRegX x
  RY -> writeCPU cpuRegY x
  RS -> writeCPU cpuRegS x
  Mem addr -> writeMem8 addr x

readMem8 :: Word16 -> NES Word8
readMem8 addr
  | addr >= 0x2000 && addr <= 0x3FFF = do
      let reg = toEnum $ fromIntegral $ addr `rem` 8
      c <- readCPU cpuCycles
      reqVar <- asks (envSync .> ppuReadReq)
      valVar <- asks (envSync .> ppuReadVal)
      liftIO $ putMVar reqVar reg
      liftIO $ takeMVar valVar
  | otherwise = do
      m <- asks envMemory
      liftIO $ read m (fromIntegral addr)

writeMem8 :: Word16 -> Word8 -> NES ()
writeMem8 addr x
  | addr >= 0x2000 && addr <= 0x3FFF = do
      let reg = toEnum $ fromIntegral $ addr `rem` 8
      c <- readCPU cpuCycles
      queue <- asks (envSync .> ppuWriteQueue)
      liftIO $ atomically $ writeTQueue queue $ PPUWrite c reg x
  | otherwise = do
      m <- asks envMemory
      liftIO $ write m (fromIntegral addr) x

readMem16 :: Word16 -> NES Word16
readMem16 addr = mkLE
  <$> readMem8 (fromIntegral addr)
  <*> readMem8 (fromIntegral addr + 1)

readMem16Pg :: Word8 -> Word8 -> NES Word16
readMem16Pg page addr = mkLE
  <$> readMem8 (fromIntegral page `shiftL` 8 .|. fromIntegral addr)
  <*> readMem8 (fromIntegral page `shiftL` 8 .|. fromIntegral (addr + 1))

writeMem16 :: Word16 -> Word16 -> NES ()
writeMem16 addr x = do
  m <- asks envMemory
  let (l, h) = deLE x
  writeMem8 (fromIntegral addr)     l
  writeMem8 (fromIntegral addr + 1) h

getFlags :: Bool -> NES Word8
getFlags b =
  readCPU cpuRegF <&> \x ->
  setClearBit 4 b $ setBit x 5

setFlags :: Word8 -> NES ()
setFlags f = writeCPU cpuRegF $ f .&. 0xCF

data CPU = CPU
  { cpuRegA   :: IORef Word8
  , cpuRegX   :: IORef Word8
  , cpuRegY   :: IORef Word8
  , cpuRegS   :: IORef Word8
  , cpuRegF   :: IORef Word8
  , cpuRegPC  :: IORef Word16
  , cpuCycles :: IORef Int
  }

data PPU = PPU
  { ppuCtrl :: IORef Word8
  , ppuMask :: IORef Word8
  , ppuVRAM :: IOVector Word8
  , ppuCycles :: IORef Int
  }

-- PPU-CPU communication {{{

data PPURegID
  = PPUCTRL
  | PPUMASK
  | PPUSTATUS
  | OAMADDR
  | OAMDATA
  | PPUSCROLL
  | PPUADDR
  | PPUDATA
  | OAMDMA
  deriving (Enum)

-- PPUWrite cycle reg val
data PPUWrite = PPUWrite Int PPURegID Word8

data NESSync = NESSync
  { ppuWriteQueue :: TQueue PPUWrite
  , ppuReadReq :: MVar PPURegID
  , ppuReadVal :: MVar Word8
  }

-- }}}

data Env = Env
  { envLogAction :: LogAction NES Message
  , envMemory    :: IOVector Word8
  , envPPU       :: PPU
  , envCPU       :: CPU
  , envSync      :: NESSync
  }

newtype NES a = NES { unNES :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance HasLog Env Message NES where
  getLogAction = envLogAction
  setLogAction new env = env { envLogAction = new }

readCPU :: (CPU -> IORef a) -> NES a
readCPU f = asks (f . envCPU) >>= liftIO . readIORef

writeCPU :: (CPU -> IORef a) -> a -> NES ()
writeCPU f x = asks (f . envCPU) >>= \r -> liftIO $ writeIORef r x

modifyCPU :: (CPU -> IORef a) -> (a -> a) -> NES ()
modifyCPU f g = asks (f . envCPU) >>= \r -> liftIO $ modifyIORef r g
