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

addAC :: AddressMode -> FDX ()
  -- ADC, Add Memory to Accumulator with Carry
  -- A + M + C -> A, C
  -- N Z C I D V
  -- + + + - - +
  --
  -- TODO: update status register
  -- TODO: add carry bit
addAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    return $! ac + b

andAC :: AddressMode -> FDX ()
  -- AND, AND Memory with Accumulator
  -- A AND M -> A
  -- N Z C I D V
  -- + + - - - -
  -- TODO: update status register
andAC mode = do
  b <- fetchOperand mode
  modifyOperand Accumulator $ \ac -> do
    return $! ac .&. b

asl :: AddressMode -> FDX ()
  -- ASL, Shift Left One Bit (Memory or Accumulator)
  -- C <- [76543210] <- 0
  -- N Z C I D V
  -- + + + - - -
  -- TODO: update status register
asl mode = modifyOperand mode $ \b -> do
  setFlag Carry (testBit b 7)
  return $! (b `shiftL` 1)

branchOn :: FDX Bool -> FDX ()
branchOn test = do
  b  <- fetchOperand Immediate
  pc <- getReg rPC
  c  <- test
  let offset = fromIntegral b :: Int8 -- make it signed
      pc'    = pc + fromIntegral offset
  when c (setPC pc')

cmp :: AddressMode -> FDX ()
cmp mode = do
  b  <- fetchOperand mode
  ac <- getReg rAC
  setFlag Carry (ac >= b)
  setFlag Zero  (ac == b)
  setFlag Negative (testBit (ac - b) 7)

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

fetchByteMem :: Word16 -> FDX Word8
fetchByteMem addr = do
  mem <- getMemory
  Mem.fetchByte addr mem

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
  low  <- fetchByteAtPC
  high <- fetchByteAtPC
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
fetchOperand Absolute  = (fetchWordAtPC >>= fetchByteMem)
fetchOperand AbsoluteX = do
  w <- fetchWordAtPC
  x <- getReg rX
  -- TODO: what does it mean to increment the address with carry?
  -- I think it means that you convert x to 16 bit and then add
  fetchByteMem (w + (toWord x))
fetchOperand AbsoluteY = do
  w <- fetchWordAtPC
  y <- getReg rY
  -- TODO: what does it mean to increment the address with carry?
  -- I think it means that you convert y to 16 bit and then add
  fetchByteMem (w + (toWord y))
fetchOperand IndirectX = do
  b <- fetchByteAtPC
  x <- getReg rX
  fetchByteMem (toWord (b + x)) -- zeropage indexed by x
fetchOperand IndirectY = do
  -- TODO: I don't understand this one at all...
  b <- fetchByteAtPC
  y <- getReg rY
  -- I think with carry means to promote b,y to Word16 then add
  fetchByteMem (toWord b + toWord y)
fetchOperand Accumulator = getReg rAC

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
  let addr = toWord (b + x) -- zeropage indexed by x
  v  <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
modifyOperand IndirectY op = do
  -- TODO: I don't understand this one at all...
  b <- fetchByteAtPC
  y <- getReg rY
  -- I think with carry means to promote b,y to Word16 then add
  let addr = toWord b + toWord y
  v  <- fetchByteMem addr
  v' <- op v
  writeByteMem addr v'
-- this does not set the status register flags
-- for the accumulator, that's is up to the caller of
-- modifyOperand
modifyOperand Accumulator op = do
  ac <- getReg rAC
  ac' <- op ac
  setAC ac'

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
  0x00 -> return () -- TODO: implement me
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
  0xC9 -> cmp Immediate
  0xC5 -> cmp Zeropage
  0xD5 -> cmp ZeropageX
  0xCD -> cmp Absolute
  0xDD -> cmp AbsoluteX
  0xD9 -> cmp AbsoluteY
  0xC1 -> cmp IndirectX
  0xD1 -> cmp IndirectY
  -- TODO: all unimplemented opcodes are nop
  _ -> do
    return ()
