{-# LANGUAGE BangPatterns #-}
module Emulator.Instructions where

import qualified Emulator.Memory as Mem
import Emulator.Registers
import Emulator.Machine

import Control.Applicative ( (<$>) )
import Control.Monad ( when )

import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe ( isJust )

getReg :: (Registers -> a) -> FDX a
getReg f = do
  rs <- getRegisters
  return $! f rs

setPC :: Word16 -> FDX ()
setPC !pc = do
  rs <- getRegisters
  setRegisters $! rs { rPC = pc }
  
setAC :: Word8 -> FDX ()
setAC !w = do
  rs <- getRegisters
  setRegisters $! rs { rAC = w }

setSP :: Word8 -> FDX ()
setSP !w = do
  rs <- getRegisters
  setRegisters $! rs { rSP = w }

setSR :: Word8 -> FDX ()
setSR !w = do
  rs <- getRegisters
  setRegisters $! rs { rSR = w }

setX :: Word8 -> FDX ()
setX !w = do
  rs <- getRegisters
  setRegisters $! rs { rX = w }

setY :: Word8 -> FDX ()
setY !w = do
  rs <- getRegisters
  setRegisters $! rs { rY = w }

addAC :: AddressMode -> FDX ()
-- TODO: update status register
-- TODO: add carry bit
addAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    return $! ac + b

sbc :: AddressMode -> FDX ()
sbc mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  c <- isFlagSet Carry
  -- TODO: update status register
  return $! ac - b - (toBit c)
 where
 toBit True  = 1
 toBit False = 0

andAC :: AddressMode -> FDX ()
-- TODO: update status register
andAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    return $! ac .&. b

asl :: AddressMode -> FDX ()
-- TODO: update status register
asl mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  return $! (b `shiftL` 1)

lsr :: AddressMode -> FDX ()
-- TODO: update status register
lsr mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  -- TODO: is this the right shiftR?
  return $! (b `shiftR` 1)

dec :: AddressMode -> FDX ()
dec mode = modifyOperand mode $ \b -> do
  let m = b - 1
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

eor :: AddressMode -> FDX ()
eor mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  let m = ac `xor` b
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

ora :: AddressMode -> FDX ()
ora mode = modifyOperand Accumulator $ \ac -> do
  b <- fetchOperand mode
  let m = ac .|. b
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

rol :: AddressMode -> FDX ()
rol mode = modifyOperand mode $ \m -> do
  let m' = m `rotateL` 1
  -- TODO: update status registers and check
  -- that this is the right rotate
  return m'

ror :: AddressMode -> FDX ()
ror mode = modifyOperand mode $ \m -> do
  let m' = m `rotateR` 1
  -- TODO: update status registers and check
  -- that this is the right rotate
  return m'

inc :: AddressMode -> FDX ()
inc mode = modifyOperand mode $ \b -> do
  let m = b + 1
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

data JMPType = AbsoluteJmp
             | IndirectJmp

jmp :: JMPType -> FDX ()
-- TODO: no idea if either of these are correct
jmp AbsoluteJmp = fetchWordAtPC >>= setPC
jmp IndirectJmp = do
  addr <- fetchWordAtPC
  pcl  <- fetchByteMem addr
  pch  <- fetchByteMem (addr + 1)
  setPC (mkWord pcl pch)

-- TODO: I think this is right
jsr :: FDX ()
jsr = do
  pch <- fetchOperand Immediate
  pcl <- fetchOperand Immediate
  pushPC
  setPC (mkWord pcl pch)

load :: AddressMode -> AddressMode -> FDX ()
load src dest = modifyOperand dest $ \_ -> do
  m <- fetchOperand src
  setFlag Negative (testBit m 7)
  setFlag Zero     (m == 0)
  return m

store :: AddressMode -> AddressMode -> FDX ()
store src dest = modifyOperand dest (const (fetchOperand src))

branchOn :: FDX Bool -> FDX ()
branchOn test = do
  b  <- fetchOperand Immediate
  pc <- getReg rPC
  c  <- test
  let offset = fromIntegral b :: Int8 -- make it signed
      pc'    = pc + fromIntegral offset
  when c (setPC pc')

pull :: FDX Word8
pull = do
  sp <- getReg rSP
  let hi  = 0x0100 :: Word16
      sp' = hi + toWord sp + 1
  b <- fetchByteMem sp'
  setSP $! sp + 1
  return b

pushReg :: (Registers -> Word8) -> FDX ()
pushReg s = do
  r  <- getReg s
  sp <- getReg rSP
  let hi   = 0x0100 :: Word16
      addr = hi + toWord sp
  writeByteMem addr r
  setSP $! sp - 1

pushPC :: FDX ()
pushPC = do
  pc <- getReg rPC
  sp <- getReg rSP
  let hi   = 0x01 :: Word16
      addr = hi + toWord sp
      pcl  = fromIntegral (pc .&. 0x00FF)
      pch  = fromIntegral (pc .&. 0xFF00)
  writeByteMem  addr    pcl
  writeByteMem (addr+1) pch

cmp :: (Registers -> Word8) -> AddressMode -> FDX ()
cmp s mode = do
  b <- fetchOperand mode
  r <- getReg s
  setFlag Carry (r >= b)
  setFlag Zero  (r == b)
  setFlag Negative (testBit (r - b) 7)

testBits :: AddressMode -> FDX ()
testBits mode = do
  b  <- fetchOperand mode
  ac <- getReg rAC
  let m7 = testBit b 7
      m6 = testBit b 6
  setFlag Negative m7
  setFlag Overflow m6
  setFlag Zero (ac .&. b == 0)

isFlagSet :: SRFlag -> FDX Bool
isFlagSet f = do
  rs <- getRegisters
  return (isJust (lookupSRFlag rs f))

setFlag :: SRFlag -> Bool -> FDX ()
setFlag f b = do
  rs <- getRegisters
  setRegisters $! setFlagBit rs b
 where
 setFlagBit rs True  = setSRFlag rs f
 setFlagBit rs False = clearSRFlag rs f

-- | Fetches a byte from the provided
-- address.
fetchByteMem :: Word16 -> FDX Word8
fetchByteMem addr = do
  mem <- getMemory
  Mem.fetchByte addr mem

-- | Fetches a word located at an address
-- stored in the zero page. That means
-- we only need an 8bit address, but we
-- also read address+1
fetchWordMem :: Word8 -> FDX Word16
fetchWordMem addr = do
  mem <- getMemory
  lo  <- Mem.fetchByte (toWord addr)     mem
  hi  <- Mem.fetchByte (toWord (addr+1)) mem
  return $! mkWord lo hi

writeByteMem :: Word16 -> Word8 -> FDX ()
writeByteMem addr b = do
  mem <- getMemory
  Mem.writeByte addr b mem

fetchByteAtPC :: FDX Word8
fetchByteAtPC = do
  pc  <- getReg rPC
  setPC $! 1+pc
  fetchByteMem pc

fetchWordAtPC :: FDX Word16
fetchWordAtPC = do
  high <- fetchByteAtPC
  low  <- fetchByteAtPC
  return $! mkWord low high

-- In this context, "Word" in an identifier name
-- means a machine word on the 6502 which is 16 bits.
-- Not to be confused with the Haskell Word type
mkWord :: Word8  -- ^ low byte
       -> Word8  -- ^ high byte
       -> Word16
mkWord !lb !hb = (hw `shiftL` 8) + lw
  where
  lw = toWord lb
  hw = toWord hb

toWord :: Word8 -> Word16
toWord = fromIntegral

fdx :: FDX ()
fdx = do
  b <- fetchByteAtPC
  execute b
  fdx

fetchOperand :: AddressMode -> FDX Word8
fetchOperand Immediate = fetchByteAtPC
fetchOperand Zeropage  = fetchByteAtPC >>= (fetchByteMem . toWord)
fetchOperand ZeropageX = do
  b <- fetchByteAtPC
  x <- getReg rX
  fetchByteMem (toWord (b + x)) -- stay on the zeropage but add x
fetchOperand ZeropageY = do
  b <- fetchByteAtPC
  y <- getReg rY
  fetchByteMem (toWord (b + y)) -- stay on the zeropage but add y
fetchOperand Absolute  = fetchWordAtPC >>= fetchByteMem
fetchOperand AbsoluteX = do
  w <- fetchWordAtPC
  x <- getReg rX
  fetchByteMem (w + (toWord x))
fetchOperand AbsoluteY = do
  w <- fetchWordAtPC
  y <- getReg rY
  fetchByteMem (w + (toWord y))
fetchOperand IndirectX = do
  b    <- fetchByteAtPC
  x    <- getReg rX
  addr <- fetchWordMem (b + x) -- zeropage index plus x
  fetchByteMem addr
fetchOperand IndirectY = do
  -- In this case, we add the value in Y to the address pointed
  -- to by the zeropage address, and then fetch the byte there.
  b    <- fetchByteAtPC
  addr <- fetchWordMem b
  y    <- getReg rY
  fetchByteMem (addr + toWord y)
fetchOperand Accumulator = getReg rAC
fetchOperand X           = getReg rX
fetchOperand Y           = getReg rY
fetchOperand SP          = getReg rSP

modifyOperand :: AddressMode -> (Word8 -> FDX Word8) -> FDX ()
modifyOperand Immediate _ = return () -- TODO: should this be an error?
modifyOperand Zeropage op = do
  b  <- fetchByteAtPC
  b' <- op b
  writeByteMem (toWord b) b'
modifyOperand ZeropageX op = do
  b <- fetchByteAtPC
  x <- getReg rX
  let addr = toWord (b + x) -- stay on the zeropage but add x 
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
modifyOperand ZeropageY op = do
  b <- fetchByteAtPC
  y <- getReg rY
  let addr = toWord (b + y) -- stay on the zeropage but add y 
  v <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
modifyOperand Absolute  op = do
  addr <- fetchWordAtPC
  b  <- fetchByteMem addr
  b' <- op b
  writeByteMem addr b'
modifyOperand AbsoluteX op = do
  w <- fetchWordAtPC
  x <- getReg rX
  -- TODO: what does it mean to increment the address with carry?
  -- I think it means that you convert x to 16 bit and then add
  let addr = w + toWord x
  b  <- fetchByteMem addr
  b' <- op b
  writeByteMem addr b'
modifyOperand AbsoluteY op = do
  w <- fetchWordAtPC
  y <- getReg rY
  -- TODO: what does it mean to increment the address with carry?
  -- I think it means that you convert y to 16 bit and then add
  let addr = w + toWord y
  b  <- fetchByteMem addr
  b' <- op b
  writeByteMem addr b'
modifyOperand IndirectX op = do
  b <- fetchByteAtPC
  x <- getReg rX
  let zeroPageAddr = b + x -- zeropage indexed by b + x
  addr <- fetchWordMem zeroPageAddr
  v    <- fetchByteMem addr
  v'   <- op v
  writeByteMem addr v'
modifyOperand IndirectY op = do
  b <- fetchByteAtPC
  y <- getReg rY
  v <- fetchWordMem b -- zeropage indexed by b, add y to result
  let addr = v + toWord y
  v  <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
-- this does not set the status register flags
-- for the accumulator, that's is up to the caller of
-- modifyOperand
modifyOperand Accumulator op = do
  ac  <- getReg rAC
  ac' <- op ac
  setAC ac'
modifyOperand X           op = do
  x  <- getReg rX
  x' <- op x
  setX x'
modifyOperand Y           op = do
  y  <- getReg rY
  y' <- op y
  setY y'
modifyOperand SP          op = do
  sp  <- getReg rSP
  sp' <- op sp
  setSP sp'

execute :: Word8 -> FDX ()
execute opc = case opc of
  -- ADC, Add Memory to Accumulator with Carry
  -- A + M + C -> A, C
  -- N Z C I D V
  -- + + + - - +
  --
  0x69 -> addAC Immediate
  0x65 -> addAC Zeropage
  0x75 -> addAC ZeropageX
  0x6D -> addAC Absolute
  0x7D -> addAC AbsoluteX
  0x79 -> addAC AbsoluteY
  0x61 -> addAC IndirectX
  0x71 -> addAC IndirectY
  -- AND, AND Memory with Accumulator
  -- A AND M -> A
  -- N Z C I D V
  -- + + - - - -
  0x29 -> andAC Immediate
  0x25 -> andAC Zeropage
  0x35 -> andAC ZeropageX
  0x2D -> andAC Absolute
  0x3D -> andAC AbsoluteX
  0x39 -> andAC AbsoluteY
  0x21 -> andAC IndirectX
  0x31 -> andAC IndirectY
  -- ASL, Shift Left One Bit (Memory of Accumulator)
  -- C <- [76543210] <- 0
  -- N Z C I D V
  -- + + + - - -
  0x0A -> asl Accumulator
  0x06 -> asl Zeropage
  0x16 -> asl ZeropageX
  0x0E -> asl Absolute
  0x1E -> asl AbsoluteX
  -- BCC, Branch on Carry Clear
  -- branch if C = 0
  -- N Z C I D V
  -- - - - - - -
  0x90 -> branchOn (not <$> isFlagSet Carry)
  -- BCS, Branch On Carry Set
  -- branch if C = 1
  -- N Z C I D V
  -- - - - - - -
  0xB0 -> branchOn (isFlagSet Carry)
  -- BEQ, Branch on Result Zero
  -- branch if Z = 1
  -- N Z C I D V
  -- - - - - - -
  0xF0 -> branchOn (isFlagSet Zero)
  -- BIT, Test Bits in Memory with Accumulator
  -- A AND M, M7 -> N, M6 -> V
  --  N Z C I D V
  -- M7 + - - - M6
  -- TODO: wikibooks listed a 3rd op code for BIT
  -- why is that?
  0x24 -> testBits Zeropage
  0x2C -> testBits Absolute
  -- BMI, Branch on Result Minus
  -- branch if N = 1
  -- N Z C I D V
  -- - - - - - -
  0x30 -> branchOn (isFlagSet Negative)
  -- BNE, Branch on Result not Zero
  -- branch if Z = 0
  -- N Z C I D V
  -- - - - - - -
  0xD0 -> branchOn (not <$> isFlagSet Zero)
  -- BPL, Branch on Resutl Plus
  -- branch if N = 0
  -- N Z C I D V
  -- - - - - - -
  0x10 -> branchOn (not <$> isFlagSet Negative)
  -- BRK, Force Break
  0x00 -> do
    _ <- fetchOperand Immediate
    pushPC
    pushReg rSR
    setFlag Interrupt True
    ll <- fetchByteMem 0xFFFE
    hh <- fetchByteMem 0xFFFF
    let pc = mkWord ll hh
    setPC pc
  -- BVC, Break on Overflow Clear
  -- branch if V = 0
  -- N Z C I D V
  -- - - - - - -
  0x50 -> branchOn (not <$> isFlagSet Overflow)
  -- BVS, Branch on Overflow Set
  -- branch if V = 1
  -- N Z C I D V
  -- - - - - - -
  0x70 -> branchOn (isFlagSet Overflow)
  -- CLC, Clear Carry flag
  -- 0 -> C
  -- N Z C I D V
  -- - - 0 - - -
  0x18 -> setFlag Carry False
  -- CLD, Clear Decimal Mode
  -- 0 -> D
  -- N Z C I D V
  -- - - - - 0 -
  0xD8 -> setFlag Decimal False
  -- CLI, Clear Interrupt Disable Bit
  -- 0 -> I
  -- N Z C I D V
  -- - - - 0 - -
  0x58 -> setFlag Interrupt False
  -- CLV, Clear Overflow Flag
  -- 0 -> V
  -- N Z C I D V
  -- - - - - - 0
  0xB8 -> setFlag Overflow False
  -- CMP, Compare Memory with Accumulator
  -- A - M
  -- N Z C I D V
  -- + + + - - -
  0xC9 -> cmp rAC Immediate
  0xC5 -> cmp rAC Zeropage
  0xD5 -> cmp rAC ZeropageX
  0xCD -> cmp rAC Absolute
  0xDD -> cmp rAC AbsoluteX
  0xD9 -> cmp rAC AbsoluteY
  0xC1 -> cmp rAC IndirectX
  0xD1 -> cmp rAC IndirectY
  -- CPX, Compare Memory and Index X
  -- X - M
  -- N Z C I D V
  -- + + + - - -
  0xE0 -> cmp rX Immediate
  0xE4 -> cmp rX Zeropage
  0xEC -> cmp rX Absolute
  -- CPY, Compare Memory and Index Y
  -- Y - M
  -- N Z C I D V
  -- + + + - - -
  0xC0 -> cmp rY Immediate
  0xC4 -> cmp rY Zeropage
  0xCC -> cmp rY Absolute
  -- DEC, Decrement Memory by One
  -- M - 1 -> M
  -- N Z C I D V
  -- + + - - - -
  0xC6 -> dec Zeropage
  0xD6 -> dec ZeropageX
  0xCE -> dec Absolute
  0xDE -> dec AbsoluteX
  -- DEX, Decrement Index X by One
  -- X - 1 -> X
  -- N Z C I D V
  -- + + - - - -
  0xCA -> dec X
  -- DEY, Decrement Index Y by One
  -- Y - 1 -> Y
  -- N Z C I D V
  -- + + - - - -
  0x88 -> dec Y
  -- EOR, Exclusive-OR Memory with Accumulator
  -- A EOR M -> A
  -- N Z C I D V
  -- + + - - - -
  0x49 -> eor Immediate
  0x45 -> eor Zeropage
  0x55 -> eor ZeropageX
  0x4D -> eor Absolute
  0x5D -> eor AbsoluteX
  0x59 -> eor AbsoluteY
  0x41 -> eor IndirectX
  0x51 -> eor IndirectY
  -- INC, Increment Memory by One
  -- M + 1 -> M
  -- N Z C I D V
  -- + + - - - -
  0xE6 -> inc Zeropage
  0xF6 -> inc ZeropageX
  0xEE -> inc Absolute
  0xFE -> inc AbsoluteX
  -- INX, Increment Index X by One
  -- X + 1 -> X
  -- N Z C I D V
  -- + + - - - -
  0xE8 -> inc X
  -- INY, Increment Index Y by One
  -- Y + 1 -> Y
  -- N Z C I D V
  -- + + - - - -
  0xC8 -> inc Y
  -- JMP, Jump to New Location
  -- (PC + 1) -> PCL
  -- (PC + 2) -> PCH
  -- N Z C I D V
  -- - - - - - -
  0x4C -> jmp AbsoluteJmp
  0x6C -> jmp IndirectJmp
  -- JSR, Jump to New Location Saving Return Address
  -- push (PC + 2)
  -- (PC + 1) -> PCL
  -- (PC + 2) -> PCH
  -- N Z C I D V
  -- - - - - - -
  0x20 -> jsr
  -- LDA, Load Accumulator with Memory
  -- M -> A
  -- N Z C I D V
  -- + + - - - -
  0xA9 -> load Immediate Accumulator 
  0xA5 -> load Zeropage  Accumulator
  0xB5 -> load ZeropageX Accumulator
  0xAD -> load Absolute  Accumulator
  0xBD -> load AbsoluteX Accumulator
  0xB9 -> load AbsoluteY Accumulator
  0xA1 -> load IndirectX Accumulator
  0xB1 -> load IndirectY Accumulator
  -- LDX, Load Index X with Memory
  -- M -> X
  -- N Z C I D V
  -- + + - - - -
  0xA2 -> load Immediate X
  0xA6 -> load Zeropage  X
  0xB6 -> load ZeropageY X
  0xAE -> load Absolute  X
  0xBE -> load AbsoluteY X
  -- LDY, Load Index Y with Memory
  -- M -> Y
  -- N Z C I D V
  -- + + - - - -
  0xA0 -> load Immediate Y
  0xA4 -> load Zeropage  Y
  0xB4 -> load ZeropageX Y
  0xAC -> load Absolute  Y
  0xBC -> load AbsoluteX Y
  -- LSR, Shift One Bit Right (Memory or Accumulator)
  -- 0 -> [76543210] -> C
  -- N Z C I D V
  -- - + + - - -
  0x4A -> lsr Accumulator
  0x46 -> lsr Zeropage
  0x56 -> lsr ZeropageX
  0x4E -> lsr Absolute
  0x5E -> lsr AbsoluteX
  -- NOP, No Operation
  -- TODO: I believe this still needs to fetch the PC
  -- to get the cycle count right and not loop
  0xEA -> return ()
  -- ORA, OR Memory with Accumulator
  -- A OR M -> A
  -- N Z C I D V
  -- + + - - - -
  0x09 -> ora Immediate
  0x05 -> ora Zeropage
  0x15 -> ora ZeropageX
  0x0D -> ora Absolute
  0x1D -> ora AbsoluteX
  0x19 -> ora AbsoluteY
  0x01 -> ora IndirectX
  0x11 -> ora IndirectY
  -- PHA, Push Accumulator on Stack
  -- push A
  -- N V C I D V
  -- - - - - - -
  0x48 -> pushReg rAC
  -- PHP, Push Processor Status on Stack
  -- push SR
  -- N Z C I D V
  -- - - - - - -
  0x08 -> pushReg rSR
  -- PLA, Pull Accumulator from Stack
  -- pull A
  -- N Z C I D V
  -- + + - - - -
  0x68 -> do
    b <- pull
    setFlag Negative (testBit b 7)
    setFlag Zero     (b == 0)
    setAC b
  -- PLP, Pull Processor Status from Stack
  -- pull SR
  -- N Z C I D V
  -- + + + + + +
  0x28 -> do
    b <- pull
    setFlag Negative (testBit b 7)
    setFlag Zero     (b == 0)
    setSR b
  -- ROL, Rotate One Bit Left (Memory or Accumulator)
  -- C <- [76543210] <- C
  -- N Z C I D V
  -- + + + - - -
  0x2A -> rol Accumulator
  0x26 -> rol Zeropage
  0x36 -> rol ZeropageX
  0x2E -> rol Absolute
  0x3E -> rol AbsoluteX
  -- ROR, Rotate One Bit Right (Memory or Accumulator)
  -- C -> [76543210] -> C
  -- N Z C I D V
  -- + + + - - -
  0x6A -> ror Accumulator
  0x66 -> ror Zeropage
  0x76 -> ror ZeropageX
  0x6E -> ror Absolute
  0x7E -> ror AbsoluteX
  -- RTI, Return from Interrupt
  -- pull SR, pull PC
  -- N Z C I D V
  -- - - - - - -
  0x40 -> do
  -- TODO: is the PC set correctly at the end?
    sr  <- pull
    setSR sr
    pch <- pull
    pcl <- pull
    setPC (mkWord pcl pch)
  -- RTS, Return from Subroutine
  -- pull PC, PC + 1 -> PC
  -- N Z C I D V
  -- - - - - - -
  0x60 -> do
  -- TODO: is the PC set correctly at the end?
    pch <- pull
    pcl <- pull
    setPC ((mkWord pcl pch) + 1)
  -- SBC, Subtract Memory from Accumulator with Borrow
  -- A - M - C -> A
  -- N Z C I D V
  -- + + + - - +
  0xE9 -> sbc Immediate
  0xE5 -> sbc Zeropage
  0xF5 -> sbc ZeropageX
  0xED -> sbc Absolute
  0xFD -> sbc AbsoluteX
  0xF9 -> sbc AbsoluteY
  0xE1 -> sbc IndirectX
  0xF1 -> sbc IndirectY
  -- SEC, Set Carry Flag
  -- 1 -> C
  -- N Z C I D V
  -- - - 1 - - -
  0x38 -> setFlag Carry True
  -- SED, Set Decimal Flag
  -- 1 -> D
  -- N Z C I D V
  -- - - - - 1 -
  0xF8 -> setFlag Decimal True
  -- SEI, Set Interrupt Disable Status
  -- 1 -> I
  -- N Z C I D V
  -- - - - 1 - -
  0x78 -> setFlag Interrupt True
  -- STA, Store Accumulator in Memory
  -- A -> M
  -- N Z C I D V
  -- - - - - - -
  0x85 -> store Accumulator Zeropage
  0x95 -> store Accumulator ZeropageX
  0x8D -> store Accumulator Absolute
  0x9D -> store Accumulator AbsoluteX
  0x99 -> store Accumulator AbsoluteY
  0x81 -> store Accumulator IndirectX
  0x91 -> store Accumulator IndirectY
  -- STX, Store Index X in Memory
  -- X -> M
  -- N Z C I D V
  -- - - - - - -
  0x86 -> store X Zeropage
  0x96 -> store X ZeropageY
  0x8E -> store X Absolute
  -- STY, Store Index Y in Memory
  -- Y -> M
  -- N Z C I D V
  -- - - - - - -
  0x84 -> store Y Zeropage
  0x94 -> store Y ZeropageX
  0x8C -> store Y Absolute
  -- TAX, Transfer Accumulator to Index X
  -- A -> X
  -- N Z C I D V
  -- + + - - - -
  0xAA -> load Accumulator X
  -- TAY, Transfer Accumulator to Index Y
  -- A -> Y
  -- N Z C I D V
  -- + + - - - -
  0xA8 -> load Accumulator Y
  -- TSX, Transfer Stack Pointer to Index x
  -- SP -> X
  -- N Z C I D V
  -- + + - - - -
  0xBA -> load SP X
  -- TXA, Transfer Index X to Accumulator
  -- X -> A
  -- N Z C I D V
  -- + + - - - -
  0x8A -> load X Accumulator
  -- TXS, Transfer Index X to Stack Register
  -- X -> SP
  -- N Z C I D V
  -- + + - - - -
  0x9A -> load X SP
  -- TYA -> Transfer Index Y to Accumulator
  -- Y -> A
  -- N Z C I D V
  -- + + - - - -
  0x98 -> load Y Accumulator

  -- TODO: all unimplemented opcodes are nop
  -- the correct thing would be to check
  -- their cycle counts
  _ -> do
    return ()
