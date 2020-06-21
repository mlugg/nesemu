module Instruction where

import qualified Data.Map as M
import Util
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.IORef
import Text.Printf
import Data.Word
import Data.Bits
import Data.Functor
import Data.List
import Common
import Colog
import Data.Text hiding (intercalate)

data Mnemonic
  = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI
  | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI
  | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA | LDX | LDY
  | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL
  | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA

  -- Unofficial opcodes
  
  | DOP | TOP | LAX | SAX | DCP | ISC | SLO | RLA
  | SRE | RRA
  
  deriving (Show)

data AddrMode
  = Implied
  | Accum
  | Immed
  | ZeroPg
  | ZeroPgX
  | ZeroPgY
  | Rel
  | Abs
  | AbsX
  | AbsY
  | Ind
  | IndX
  | IndY

operandLen :: AddrMode -> Int
operandLen Implied = 0
operandLen Accum   = 0
operandLen Immed   = 1
operandLen ZeroPg  = 1
operandLen ZeroPgX = 1
operandLen ZeroPgY = 1
operandLen Rel     = 1
operandLen Abs     = 2
operandLen AbsX    = 2
operandLen AbsY    = 2
operandLen Ind     = 2
operandLen IndX    = 1
operandLen IndY    = 1

data Opcode = Opcode Word8 Mnemonic AddrMode

pPrintOperand :: AddrMode -> NES String
pPrintOperand am =
  asks (envCPU .> cpuRegPC) >>= readRef >>= \pc ->
  case am of
    Implied -> pure   ""
    Accum   -> pure   "A"
    Immed   -> printf "#$%02X"    <$> readMem8  (pc-1)
    ZeroPg  -> printf "$%02X"     <$> readMem8  (pc-1)
    ZeroPgX -> printf "$%02X,X"   <$> readMem8  (pc-1)
    ZeroPgY -> printf "$%02X,Y"   <$> readMem8  (pc-1)
    Rel     -> printf "$%02X"     <$> readMem8  (pc-1)
    Abs     -> printf "$%04X"     <$> readMem16 (pc-2)
    AbsX    -> printf "$%04X,X"   <$> readMem16 (pc-2)
    AbsY    -> printf "$%04X,Y"   <$> readMem16 (pc-2)
    Ind     -> printf "($%04X)"   <$> readMem16 (pc-2)
    IndX    -> printf "($%02X,X)" <$> readMem8  (pc-1)
    IndY    -> printf "($%02X),Y" <$> readMem8  (pc-1)


pPrintOpcode :: Opcode -> NES String
pPrintOpcode (Opcode _ op am) = printf "%s %s" (show op) <$> pPrintOperand am

opMap :: M.Map Word8 Opcode
opMap = let f x mn am = (x, Opcode x mn am) in
  M.fromList
    [ f 0x69 ADC Immed,   f 0x65 ADC ZeroPg,  f 0x75 ADC ZeroPgX
    , f 0x6D ADC Abs,     f 0x7D ADC AbsX,    f 0x79 ADC AbsY
    , f 0x61 ADC IndX,    f 0x71 ADC IndY

    , f 0x29 AND Immed,   f 0x25 AND ZeroPg,  f 0x35 AND ZeroPgX
    , f 0x2D AND Abs,     f 0x3D AND AbsX,    f 0x39 AND AbsY
    , f 0x21 AND IndX,    f 0x31 AND IndY

    , f 0x0A ASL Accum,   f 0x06 ASL ZeroPg,  f 0x16 ASL ZeroPgX
    , f 0x0E ASL Abs,     f 0x1E ASL AbsX

    , f 0x24 BIT ZeroPg,  f 0x2C BIT Abs

    , f 0x00 BRK Implied

    , f 0x90 BCC Rel,     f 0xB0 BCS Rel,     f 0xF0 BEQ Rel
    , f 0x30 BMI Rel,     f 0xD0 BNE Rel,     f 0x10 BPL Rel
    , f 0x50 BVC Rel,     f 0x70 BVS Rel

    , f 0x18 CLC Implied, f 0x58 CLI Implied, f 0xD8 CLD Implied
    , f 0xB8 CLV Implied

    , f 0x38 SEC Implied, f 0x78 SEI Implied, f 0xF8 SED Implied

    , f 0xC9 CMP Immed,   f 0xC5 CMP ZeroPg,  f 0xD5 CMP ZeroPgX
    , f 0xCD CMP Abs,     f 0xDD CMP AbsX,    f 0xD9 CMP AbsY
    , f 0xC1 CMP IndX,    f 0xD1 CMP IndY

    , f 0xE0 CPX Immed,   f 0xE4 CPX ZeroPg,  f 0xEC CPX Abs
    
    , f 0xC0 CPY Immed,   f 0xC4 CPY ZeroPg,  f 0xCC CPY Abs

    , f 0xC6 DEC ZeroPg,  f 0xD6 DEC ZeroPgX, f 0xCE DEC Abs
    , f 0xDE DEC AbsX

    , f 0xCA DEX Implied, f 0x88 DEY Implied

    , f 0x49 EOR Immed,   f 0x45 EOR ZeroPg,  f 0x55 EOR ZeroPgX
    , f 0x4D EOR Abs,     f 0x5D EOR AbsX,    f 0x59 EOR AbsY
    , f 0x41 EOR IndX,    f 0x51 EOR IndY

    , f 0xE6 INC ZeroPg,  f 0xF6 INC ZeroPgX, f 0xEE INC Abs
    , f 0xFE INC AbsX

    , f 0xE8 INX Implied, f 0xC8 INY Implied

    , f 0x4C JMP Abs,     f 0x6C JMP Ind

    , f 0x20 JSR Abs

    , f 0xA9 LDA Immed,   f 0xA5 LDA ZeroPg,  f 0xB5 LDA ZeroPgX
    , f 0xAD LDA Abs,     f 0xBD LDA AbsX,    f 0xB9 LDA AbsY
    , f 0xA1 LDA IndX,    f 0xB1 LDA IndY

    , f 0xA2 LDX Immed,   f 0xA6 LDX ZeroPg,  f 0xB6 LDX ZeroPgY
    , f 0xAE LDX Abs,     f 0xBE LDX AbsY

    , f 0xA0 LDY Immed,   f 0xA4 LDY ZeroPg,  f 0xB4 LDY ZeroPgX
    , f 0xAC LDY Abs,     f 0xBC LDY AbsX

    , f 0x4A LSR Accum,   f 0x46 LSR ZeroPg,  f 0x56 LSR ZeroPgX
    , f 0x4E LSR Abs,     f 0x5E LSR AbsX

    , f 0xEA NOP Implied

    , f 0x09 ORA Immed,   f 0x05 ORA ZeroPg,  f 0x15 ORA ZeroPgX
    , f 0x0D ORA Abs,     f 0x1D ORA AbsX,    f 0x19 ORA AbsY
    , f 0x01 ORA IndX,    f 0x11 ORA IndY

    , f 0x48 PHA Implied, f 0x08 PHP Implied

    , f 0x68 PLA Implied, f 0x28 PLP Implied

    , f 0x2A ROL Accum,   f 0x26 ROL ZeroPg,  f 0x36 ROL ZeroPgX
    , f 0x2E ROL Abs,     f 0x3E ROL AbsX

    , f 0x6A ROR Accum,   f 0x66 ROR ZeroPg,  f 0x76 ROR ZeroPgX
    , f 0x6E ROR Abs,     f 0x7E ROR AbsX

    , f 0x40 RTI Implied

    , f 0x60 RTS Implied

    , f 0xE9 SBC Immed,   f 0xE5 SBC ZeroPg,  f 0xF5 SBC ZeroPgX
    , f 0xED SBC Abs,     f 0xFD SBC AbsX,    f 0xF9 SBC AbsY
    , f 0xE1 SBC IndX,    f 0xF1 SBC IndY

    , f 0x85 STA ZeroPg,  f 0x95 STA ZeroPgX, f 0x8D STA Abs
    , f 0x9D STA AbsX,    f 0x99 STA AbsY,    f 0x81 STA IndX
    , f 0x91 STA IndY

    , f 0x86 STX ZeroPg,  f 0x96 STX ZeroPgY, f 0x8E STX Abs

    , f 0x84 STY ZeroPg,  f 0x94 STY ZeroPgX, f 0x8C STY Abs

    , f 0xAA TAX Implied, f 0xA8 TAY Implied, f 0xBA TSX Implied
    , f 0x8A TXA Implied, f 0x98 TYA Implied, f 0x9A TXS Implied

    -- Unofficial opcodes

    , f 0x04 DOP ZeroPg,  f 0x14 DOP ZeroPgX, f 0x34 DOP ZeroPgX
    , f 0x44 DOP ZeroPg,  f 0x54 DOP ZeroPgX, f 0x64 DOP ZeroPg
    , f 0x74 DOP ZeroPgX, f 0x80 DOP Immed,   f 0x82 DOP Immed
    , f 0x89 DOP Immed,   f 0xC2 DOP Immed,   f 0xD4 DOP ZeroPgX
    , f 0xE2 DOP Immed,   f 0xF4 DOP ZeroPgX

    , f 0x1A NOP Implied, f 0x3A NOP Implied, f 0x5A NOP Implied
    , f 0x7A NOP Implied, f 0xDA NOP Implied, f 0xFA NOP Implied

    , f 0x0C TOP Abs,     f 0x1C TOP AbsX,    f 0x3C TOP AbsX
    , f 0x5C TOP AbsX,    f 0x7C TOP AbsX,    f 0xDC TOP AbsX
    , f 0xFC TOP AbsX

    , f 0xA7 LAX ZeroPg,  f 0xB7 LAX ZeroPgY, f 0xAF LAX Abs
    , f 0xBF LAX AbsY,    f 0xA3 LAX IndX,    f 0xB3 LAX IndY

    , f 0x87 SAX ZeroPg,  f 0x97 SAX ZeroPgY, f 0x83 SAX IndX
    , f 0x8F SAX Abs

    , f 0xEB SBC Immed

    , f 0xC7 DCP ZeroPg,  f 0xD7 DCP ZeroPgX, f 0xCF DCP Abs
    , f 0xDF DCP AbsX,    f 0xDB DCP AbsY,    f 0xC3 DCP IndX
    , f 0xD3 DCP IndY

    , f 0xE7 ISC ZeroPg,  f 0xF7 ISC ZeroPgX, f 0xEF ISC Abs
    , f 0xFF ISC AbsX,    f 0xFB ISC AbsY,    f 0xE3 ISC IndX
    , f 0xF3 ISC IndY

    , f 0x07 SLO ZeroPg,  f 0x17 SLO ZeroPgX, f 0x0F SLO Abs
    , f 0x1F SLO AbsX,    f 0x1B SLO AbsY,    f 0x03 SLO IndX
    , f 0x13 SLO IndY

    , f 0x27 RLA ZeroPg,  f 0x37 RLA ZeroPgX, f 0x2F RLA Abs
    , f 0x3F RLA AbsX,    f 0x3B RLA AbsY,    f 0x23 RLA IndX
    , f 0x33 RLA IndY

    , f 0x47 SRE ZeroPg,  f 0x57 SRE ZeroPgX, f 0x4F SRE Abs
    , f 0x5F SRE AbsX,    f 0x5B SRE AbsY,    f 0x43 SRE IndX
    , f 0x53 SRE IndY

    , f 0x67 RRA ZeroPg,  f 0x77 RRA ZeroPgX, f 0x6F RRA Abs
    , f 0x7F RRA AbsX,    f 0x7B RRA AbsY,    f 0x63 RRA IndX
    , f 0x73 RRA IndY
    ]

enforceImplied :: AddrMode -> NES ()
enforceImplied m = case m of
  Implied -> pure ()
  _       -> error "todo"

evalOperand :: AddrMode -> NES Word8
evalOperand m = case m of
  Immed -> asks (envCPU .> cpuRegPC) >>= readRef >>= \pc -> readMem8 (pc-1)
  _     -> evalLOperand m >>= readVal

evalLOperand :: AddrMode -> NES DataLoc
evalLOperand m = case m of
  Implied -> error "todo"
  Immed   -> error "todo"
  Accum   -> pure RA
  _       -> Mem <$> evalMemOperand m

evalMemOperand :: AddrMode -> NES Word16
evalMemOperand m = asks (envCPU .> cpuRegPC) >>= readRef >>= \pc -> case m of
  ZeroPg  -> cvt16 <$> readMem8 (pc-1)
  ZeroPgX -> cvt16 <$> ((+) <$> readMem8 (pc-1) <*> readVal RX)
  ZeroPgY -> cvt16 <$> ((+) <$> readMem8 (pc-1) <*> readVal RY)
  Abs     -> readMem16 (pc-2)
  AbsX    -> ((+) <$> readMem16 (pc-2) <*> (cvt16 <$> readVal RX))
  AbsY    -> ((+) <$> readMem16 (pc-2) <*> (cvt16 <$> readVal RY))
  Ind     -> (readMem16 (pc-2) >>= \a -> readMem16Pg (fromIntegral $ a `shiftR` 8) (fromIntegral a))
  IndX    -> ((+) <$> readMem8 (pc-1) <*> readVal RX >>= readMem16Pg 0)
  IndY    -> ((+) <$> (readMem8 (pc-1) >>= readMem16Pg 0) <*> (cvt16 <$> readVal RY))
  Rel     -> fromIntegral . (fromIntegral pc +) . cvt32S . sign8 <$> readMem8 (pc-1)
  _       -> error "todo"

setFlagBit :: Int -> Bool -> NES ()
setFlagBit b v =
  getFlags False >>= \old ->
  let new = setClearBit b v old in
  setFlags new

testFlagBit :: Int -> NES Bool
testFlagBit b = getFlags True <&> \f -> testBit f b

updateFlags :: Word8 -> NES ()
updateFlags val =
  getFlags False >>= \old ->
  let new = setClearBit 1 (val == 0) $ setClearBit 7 (testBit val 7) old in
  setFlags new

getModeCycles :: AddrMode -> Int
getModeCycles m = case m of
  Implied -> 0
  Immed   -> 2
  Accum   -> 0
  ZeroPg  -> 3
  ZeroPgX -> 4
  ZeroPgY -> 4
  Rel     -> 0
  Abs     -> 4
  AbsX    -> 4
  AbsY    -> 4
  Ind     -> 6
  IndX    -> 6
  IndY    -> 5

getPageCrossPenalty :: Opcode -> NES Int
getPageCrossPenalty (Opcode _ m am) =
  if not $ hasPageCrossPenalty m
  then case am of
    AbsX -> pure 1
    AbsY -> pure 1
    IndY -> pure 1
    _    -> pure 0
  else asks (envCPU .> cpuRegPC) >>= readRef >>= \pc ->
  case am of
    AbsX -> do
      a <- cvt8 <$> readMem16 (pc-2)
      x <- readVal RX
      pure $ if a+x < x then 1 else 0
    AbsY -> do
      a <- cvt8 <$> readMem16 (pc-2)
      y <- readVal RY
      pure $ if a+y < a then 1 else 0
    IndY -> do
      a <- (readMem8 (pc-1) >>= readMem16 . cvt16) <&> cvt8
      y <- readVal RY
      pure $ if a+y < a then 1 else 0
    _    -> pure 0

hasPageCrossPenalty :: Mnemonic -> Bool
hasPageCrossPenalty m = case m of
  { ADC -> True; AND -> True; CMP -> True
  ; EOR -> True; LDA -> True; LDX -> True
  ; LDY -> True; ORA -> True; SBC -> True
  ; TOP -> True; LAX -> True
  ; _   -> False }

getTotalModeCycles :: Opcode -> NES Int
getTotalModeCycles o@(Opcode _ _ am) = getPageCrossPenalty o <&> (+ getModeCycles am)

advanceCycles :: Int -> NES ()
advanceCycles x = asks envCycles >>= liftIO . flip modifyIORef' (+x)

differentPage :: Word16 -> NES Bool
differentPage addr = (asks (envCPU .> cpuRegPC) >>= readRef) <&> \pc ->
  (pc .&. 0xff00) /= (addr .&. 0xff00)

-- All branching ops are basically the same code, so we take out the
-- common pattern
branchOp :: AddrMode -> Int -> Bool -> NES ()
branchOp mode bit val =
  testFlagBit bit >>= \v ->
  advanceCycles 2 *>
  if v == val
  then
    evalMemOperand mode >>= \addr -> 
    differentPage addr  >>= \diffPg ->
    let c = if diffPg then 2 else 1 in
    advanceCycles c *>
    asks (envCPU .> cpuRegPC) >>= flip writeRef addr
  else pure ()

-- All transfer ops are basically the same code too
transferOp :: DataLoc -> DataLoc -> Bool -> NES ()
transferOp src dst doFlags =
  readVal src >>= \val ->
  writeVal dst val *>
  advanceCycles 2  *>
  if doFlags
  then updateFlags val
  else pure ()

stackPush8 :: Word8 -> NES ()
stackPush8 val =
  readVal RS >>= \stack ->
  writeMem8 (0x0100 + cvt16 stack) val *>
  writeVal RS (stack-1)

stackPush16 :: Word16 -> NES ()
stackPush16 val =
  readVal RS >>= \stack ->
  writeMem16 (0x0100 + cvt16 (stack-1)) val *>
  writeVal RS (stack-2)

stackPop8 :: NES Word8
stackPop8 =
  readVal RS >>= \stack ->
  readMem8 (0x0100 + cvt16 (stack+1)) <*
  writeVal RS (stack+1)

stackPop16 :: NES Word16
stackPop16 =
  readVal RS >>= \stack ->
  readMem16 (0x0100 + cvt16 (stack+1)) <*
  writeVal RS (stack+2)

adcOp :: Word8 -> NES ()
adcOp m =
  readVal RA       >>= \a ->
  testFlagBit 0    >>= \carryIn ->
  let sum = cvt16 m + cvt16 a + (if carryIn then 1 else 0) in
  writeVal RA (cvt8 sum)       *>
  setFlagBit 0 (sum > 0xFF)    *>
  setFlagBit 1 (cvt8 sum == 0) *>
  setFlagBit 6 (complement (a `xor` m) .&. (a `xor` cvt8 sum) .&. 0x80 /= 0) *>
  setFlagBit 7 (testBit sum 7)

-- By the time this is run, the program counter has ALREADY BEEN
-- INCREMENTED to the next instruction
runOp :: Opcode -> NES ()
runOp o@(Opcode opcode mn mode) =
  (getTotalModeCycles o >>= advanceCycles) *>
  case mn of
    ADC ->
      evalOperand mode >>= adcOp

    AND ->
      evalOperand mode >>= \x ->
      readVal RA       >>= \old ->
      let new = old .&. x in
      writeVal RA new *>
      updateFlags new

    ASL ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      let new = old `shiftL` 1 in
      writeVal loc new *>
      setFlagBit 0 (testBit old 7) *>
      setFlagBit 1 (new == 0)      *>
      setFlagBit 7 (testBit new 7) *>
      advanceCycles 2

    BCC -> branchOp mode 0 False
    BCS -> branchOp mode 0 True
    BNE -> branchOp mode 1 False
    BEQ -> branchOp mode 1 True
    BVC -> branchOp mode 6 False
    BVS -> branchOp mode 6 True
    BPL -> branchOp mode 7 False
    BMI -> branchOp mode 7 True

    BIT ->
      evalOperand mode >>= \x ->
      readVal RA       >>= \a ->
      let res = a .&. x in
      setFlagBit 1 (res == 0)    *>
      setFlagBit 6 (testBit x 6) *>
      setFlagBit 7 (testBit x 7)

    BRK ->
      interrupt 0xFFFE True

    CLC -> setFlagBit 0 False *> advanceCycles 2
    SEC -> setFlagBit 0 True  *> advanceCycles 2
    CLI -> setFlagBit 2 False *> advanceCycles 2
    SEI -> setFlagBit 2 True  *> advanceCycles 2
    CLV -> setFlagBit 6 False *> advanceCycles 2
    CLD -> setFlagBit 3 False *> advanceCycles 2
    SED -> setFlagBit 3 True  *> advanceCycles 2

    CMP ->
      evalOperand mode >>= \m ->
      readVal RA       >>= \a ->
      let val = a - m in
      updateFlags val *>
      setFlagBit 0 (a >= m)

    CPX ->
      evalOperand mode >>= \m ->
      readVal RX       >>= \x ->
      let val = x - m in
      updateFlags val *>
      setFlagBit 0 (x >= m)

    CPY ->
      evalOperand mode >>= \m ->
      readVal RY       >>= \y ->
      let val = y - m in
      updateFlags val *>
      setFlagBit 0 (y >= m)

    DEC ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      let new = old - 1 in
      updateFlags new  *>
      writeVal loc new *>
      advanceCycles 2

    DEX ->
      readVal RX >>= \old ->
      let new = old - 1 in
      updateFlags new *>
      writeVal RX new *>
      advanceCycles 2

    DEY ->
      readVal RY >>= \old ->
      let new = old - 1 in
      updateFlags new *>
      writeVal RY new *>
      advanceCycles 2

    EOR ->
      evalOperand mode >>= \m ->
      readVal RA       >>= \a ->
      let val = a `xor` m in
      updateFlags val *>
      writeVal RA val

    INC ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      let new = old + 1 in
      updateFlags new  *>
      writeVal loc new *>
      advanceCycles 2

    INX ->
      readVal RX >>= \old ->
      let new = old + 1 in
      updateFlags new *>
      writeVal RX new *>
      advanceCycles 2

    INY ->
      readVal RY >>= \old ->
      let new = old + 1 in
      updateFlags new *>
      writeVal RY new *>
      advanceCycles 2

    JMP ->
      evalMemOperand mode       >>= \addr ->
      asks (envCPU .> cpuRegPC) >>= \pcRef ->
      writeRef pcRef addr *>
      advanceCycles (-1)  -- An annoying case: JMP is a bit faster than everything else

    JSR ->
      evalMemOperand mode       >>= \addr ->
      asks (envCPU .> cpuRegPC) >>= \pcRef ->
      readRef pcRef             >>= \ret ->
      stackPush16 (ret - 1) *>
      writeRef pcRef addr   *>
      advanceCycles 2

    LDA ->
      evalOperand mode >>= \val ->
      writeVal RA val *>
      updateFlags val

    LDX ->
      evalOperand mode >>= \val ->
      writeVal RX val *>
      updateFlags val

    LDY ->
      evalOperand mode >>= \val ->
      writeVal RY val *>
      updateFlags val
      
    LSR ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      let new = old `shiftR` 1 in
      writeVal loc new *>
      updateFlags new  *>
      setFlagBit 0 (testBit old 0) *>
      advanceCycles 2

    NOP ->
      advanceCycles 2

    DOP ->
      pure ()

    TOP ->
      pure ()

    ORA ->
      evalOperand mode >>= \m ->
      readVal RA       >>= \a ->
      let res = m .|. a in
      writeVal RA res *>
      updateFlags res

    PHA ->
      (readVal RA >>= stackPush8) *>
      advanceCycles 3

    PHP ->
      (getFlags True >>= stackPush8) *>
      advanceCycles 3

    PLA ->
      stackPop8 >>= \a ->
      writeVal RA a *>
      updateFlags a *>
      advanceCycles 4

    PLP ->
      (stackPop8 >>= setFlags) *>
      advanceCycles 4

    ROL ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      testFlagBit 0     >>= \carryIn ->
      let new = setClearBit 0 carryIn $ old `shiftL` 1 in
      writeVal loc new *>
      readVal RA  >>= \acc ->
      setFlagBit 0 (testBit old 7) *>
      setFlagBit 1 (acc == 0)      *>
      setFlagBit 7 (testBit new 7) *>
      advanceCycles 2

    ROR ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      testFlagBit 0     >>= \carryIn ->
      let new = setClearBit 7 carryIn $ old `shiftR` 1 in
      writeVal loc new *>
      readVal RA  >>= \acc ->
      setFlagBit 0 (testBit old 0) *>
      setFlagBit 1 (acc == 0)      *>
      setFlagBit 7 (testBit new 7) *>
      advanceCycles 2

    RTI ->
      asks (envCPU .> cpuRegPC) >>= \pcRef ->
      (stackPop8  >>= setFlags)       *>
      (stackPop16 >>= writeRef pcRef) *>
      advanceCycles 6

    RTS ->
      stackPop16                >>= \addr ->
      asks (envCPU .> cpuRegPC) >>= \pcRef ->
      writeRef pcRef (addr+1) *>
      advanceCycles 6

    SBC ->
      evalOperand mode >>= adcOp . complement

    STA ->
      evalLOperand mode >>= \loc ->
      readVal RA        >>= \a ->
      writeVal loc a

    STX ->
      evalLOperand mode >>= \loc ->
      readVal RX        >>= \x ->
      writeVal loc x

    STY ->
      evalLOperand mode >>= \loc ->
      readVal RY        >>= \y ->
      writeVal loc y

    TAX ->
      transferOp RA RX True

    TAY ->
      transferOp RA RY True

    TSX ->
      transferOp RS RX True

    TXA ->
      transferOp RX RA True

    TXS ->
      transferOp RX RS False

    TYA ->
      transferOp RY RA True

    LAX ->
      evalOperand mode >>= \val ->
      writeVal RA val *>
      writeVal RX val *>
      updateFlags val

    SAX ->
      evalLOperand mode >>= \loc ->
      readVal RA >>= \a ->
      readVal RX >>= \x ->
      let res = a .&. x in
      writeVal loc res

    DCP ->
      evalLOperand mode >>= \loc ->
      readVal loc >>= \old ->
      readVal RA  >>= \a ->
      let new = old - 1
          flagVal = a - new
      in
      writeVal loc new *>
      updateFlags flagVal *>
      setFlagBit 0 (a >= new) *>
      advanceCycles 2

    ISC ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      let new = old + 1 in
      writeVal loc new       *>
      adcOp (complement new) *>
      advanceCycles 2

    SLO ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      readVal RA        >>= \acc ->
      let new = old `shiftL` 1
          accRes = new .|. acc
      in
      writeVal loc new    *>
      writeVal RA  accRes *>
      setFlagBit 0 (testBit old 7) *>
      updateFlags accRes *>
      advanceCycles 2

    RLA ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      testFlagBit 0     >>= \carryIn ->
      readVal RA        >>= \acc ->
      let new = setClearBit 0 carryIn $ old `shiftL` 1
          accNew = acc .&. new
      in
      writeVal loc new *>
      writeVal RA accNew *>
      setFlagBit 0 (testBit old 7) *>
      updateFlags accNew *>
      advanceCycles 2

    SRE ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      readVal RA        >>= \acc ->
      let new = old `shiftR` 1
          accNew = acc `xor` new
      in
      writeVal loc new *>
      writeVal RA accNew *>
      setFlagBit 0 (testBit old 0) *>
      updateFlags accNew *>
      advanceCycles 2

    RRA ->
      evalLOperand mode >>= \loc ->
      readVal loc       >>= \old ->
      testFlagBit 0     >>= \carryIn ->
      let new = setClearBit 7 carryIn $ old `shiftR` 1 in
      writeVal loc new *>
      setFlagBit 0 (testBit old 0) *>
      adcOp new *>
      advanceCycles 2

triggerIRQ :: NES ()
triggerIRQ = interrupt 0xFFFE False

triggerNMI :: NES ()
triggerNMI = interrupt 0xFFFA False

interrupt :: Word16 -> Bool -> NES ()
interrupt vec soft =
  asks (envCPU .> cpuRegPC) >>= \pcRef ->
  (readRef pcRef >>= stackPush16) *>
  (getFlags soft >>= stackPush8)  *>
  setFlagBit 2 True *>
  readMem16 vec >>= \addr ->
  writeRef pcRef addr *>
  advanceCycles 7

logInsn :: Word16 -> Opcode -> NES ()
logInsn pc (Opcode _ mn mode) =
  pPrintOperand mode >>= \opStr ->
  asks (envCycles) >>= readRef >>= \cyc ->
  readVal RA >>= \a ->
  readVal RX >>= \x ->
  readVal RY >>= \y ->
  readVal RS >>= \s ->
  getFlags False >>= \p ->
  let n = fromIntegral $ operandLen mode + 1
      addrs = [pc..pc+n-1]
  in
  readMem8 `mapM` addrs >>= \rawCode ->
  let code = intercalate " " $ printf "%.2X" <$> rawCode in

  logDebug $ pack $
    printf "%04X  %-8s  %-15s A:%.2X X:%.2X Y:%.2X P:%.2X S:%.2X CYC:%d"
      pc
      code
      (show mn <> " " <> opStr)
      a
      x
      y
      p
      s
      cyc

doInsn :: NES ()
doInsn = 
  asks (envCPU .> cpuRegPC) >>= \pcRef ->
  readRef pcRef >>= \pc ->
  readMem8 pc   >>= \code ->
  let o@(Opcode _ _ mode) = opMap M.! code in
  writeRef pcRef (pc + 1 + fromIntegral (operandLen mode)) *>
  logInsn pc o *>
  runOp o
