{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Common where

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
  RA -> asks (envCPU .> cpuRegA) >>= readRef
  RX -> asks (envCPU .> cpuRegX) >>= readRef
  RY -> asks (envCPU .> cpuRegY) >>= readRef
  RS -> asks (envCPU .> cpuRegS) >>= readRef
  Mem addr -> readMem8 addr

writeVal :: DataLoc -> Word8 -> NES ()
writeVal l x = case l of
  RA -> asks (envCPU .> cpuRegA) >>= flip writeRef x
  RX -> asks (envCPU .> cpuRegX) >>= flip writeRef x
  RY -> asks (envCPU .> cpuRegY) >>= flip writeRef x
  RS -> asks (envCPU .> cpuRegS) >>= flip writeRef x
  Mem addr -> writeMem8 addr x

readMem8 :: Word16 -> NES Word8
readMem8 addr =
  asks envMemory >>= \m ->
  liftIO (read m (fromIntegral addr))

readMem16 :: Word16 -> NES Word16
readMem16 addr =
  asks envMemory >>= \m ->
  liftIO (mkLE
    <$> read m (fromIntegral addr)
    <*> read m (fromIntegral addr + 1)
  )

readMem16Pg :: Word8 -> Word8 -> NES Word16
readMem16Pg page addr =
  asks envMemory >>= \m ->
  liftIO (mkLE
    <$> read m (fromIntegral page `shiftL` 8 .|. fromIntegral addr)
    <*> read m (fromIntegral page `shiftL` 8 .|. fromIntegral (addr + 1))
  )

writeMem8 :: Word16 -> Word8 -> NES ()
writeMem8 addr x =
  asks envMemory >>= \m -> liftIO $
  write m (fromIntegral addr) x

writeMem16 :: Word16 -> Word16 -> NES ()
writeMem16 addr x =
  asks envMemory >>= \m -> liftIO $
  let (l, h) = deLE x in
  write m (fromIntegral addr)     l *>
  write m (fromIntegral addr + 1) h

getFlags :: Bool -> NES Word8
getFlags b =
  (asks (envCPU .> cpuRegF) >>= readRef) <&> \x ->
  setClearBit 4 b $ setBit x 5

setFlags :: Word8 -> NES ()
setFlags f = asks (envCPU .> cpuRegF) >>= flip writeRef (f .&. 0xCF)

data CPU = CPU
  { cpuRegA  :: IORef Word8
  , cpuRegX  :: IORef Word8
  , cpuRegY  :: IORef Word8
  , cpuRegS  :: IORef Word8
  , cpuRegF  :: IORef Word8
  , cpuRegPC :: IORef Word16
  }

data Env = Env
  { envLogAction :: LogAction NES Message
  , envMemory    :: IOVector Word8
  , envCPU       :: CPU
  , envCycles    :: IORef Int
  }

newtype NES a = NES { unNES :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance HasLog Env Message NES where
  getLogAction = envLogAction
  setLogAction new env = env { envLogAction = new }

readRef :: IORef a -> NES a
readRef = liftIO . readIORef

writeRef :: IORef a -> a -> NES ()
writeRef x = liftIO . writeIORef x
