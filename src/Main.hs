module Main where

import Data.Binary.Strict.Get
import Data.Word
import Text.Printf
import Data.Functor
import Control.Monad.Reader
import Data.Vector.Unboxed.Mutable as V
import Colog
import Data.IORef
import qualified Data.ByteString as BS

import Common
import Instruction
import Util
import RomParse

import System.CPUTime

test :: NES ()
test =
  liftIO getCPUTime >>= \start ->
  testLoop *>
  liftIO getCPUTime >>= \end ->
  let diff = fromIntegral (end-start) / 10^12 in
  liftIO $ printf "computation took %0.3f secs\n" (diff::Double)

testLoop :: NES ()
testLoop =
  doInsn *>
  asks (envCPU .> cpuRegPC) >>= readRef >>= \pc ->
  if pc /= 0xC66E
  then testLoop
  else
    readMem8 0x0002 >>= \a ->
    readMem8 0x0003 >>= \b ->
    (asks envCycles >>= readRef) >>= \c ->
    liftIO (printf "Exec finished! 0x0002 was %d, 0x0003 was %d, did %d cycles\n" a b c)

mkNES :: IOVector Word8 -> IO Env
mkNES mem =
  Env
      simpleMessageAction
--      (LogAction $ const $ pure ())
    mem
  <$> (CPU
        <$> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0xFD
        <*> newIORef 0x04
        <*> newIORef 0xC000)
  <*> newIORef 7

dumpMem :: NES ()
dumpMem =
  asks envMemory >>= \m -> (
    [0x0000..0xffff::Word16] `forM` \a ->
    liftIO (V.read m (fromIntegral a)) >>= \x ->
    when (a `mod` 0x10 == 0) (liftIO $ printf "\n%.4X:" a) *>
    liftIO (printf " %.2X" x)
  ) *> liftIO (putStr "\n")

main :: IO ()
main =
  liftIO (BS.readFile "./nestest.nes") >>= \d ->
  let (Right rom, _) = runGet parseRom d in
  mkMem rom >>= mkNES >>= \initNes ->
  runReaderT (unNES test) initNes
