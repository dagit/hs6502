{-# LANGUAGE BangPatterns #-}
module Emulator.Instructions where

import qualified Emulator.Memory as Mem
import Emulator.Registers
import Emulator.Machine

import Data.Word
import Data.Bits

getReg :: (Registers -> a) -> FDX a
getReg f = do
  rs <- getRegisters
  return $! f rs

getPC :: FDX Word16
getPC = getReg rPC

setPC :: Word16 -> FDX ()
setPC !pc = do
  rs <- getRegisters
  setRegisters $! rs { rPC = pc }
  
incPC :: FDX ()
incPC = do
  pc <- getPC
  setPC $! 1+pc

getAC :: FDX Word8
getAC = getReg rAC

setAC :: Word8 -> FDX ()
setAC !w = do
  rs <- getRegisters
  setRegisters $! rs { rAC = w }

incACBy :: FDX Word8 -> FDX ()
  -- ADC, Add Memory to Accumulator with Carry
  -- A + M + C -> A, C
  -- N Z C I D V
  -- + + + - - +
  --
incACBy by = do
  ac <- getAC
  m <- by 
  -- TODO: update status register
  -- TODO: add carry bit
  setAC $! ac + m

getX :: FDX Word8
getX = getReg rX

getY :: FDX Word8
getY = getReg rY

fetchByteMem :: Word16 -> FDX Word8
fetchByteMem addr = do
  mem <- getMemory
  Mem.fetchByte addr mem

fetchByteAtPC :: FDX Word8
fetchByteAtPC = do
  pc  <- getPC
  incPC
  fetchByteMem pc

fetchWordAtPC :: FDX Word16
fetchWordAtPC = do
  low  <- fetchByteAtPC
  high <- fetchByteAtPC
  return $! mkWord low high

fetchBytesAtPC :: Int -> FDX [Word8]
fetchBytesAtPC 0        = return []
fetchBytesAtPC numBytes = do
  b  <- fetchByteAtPC
  bs <- fetchBytesAtPC (pred numBytes)
  return $! b : bs

mkWord :: Word8  -- ^ low byte
       -> Word8  -- ^ high byte
       -> Word16
mkWord !lb !hb = (hw `shiftL` 8) + lw
  where
  lw = fromIntegral lb :: Word16
  hw = fromIntegral hb :: Word16

fdx :: FDX ()
fdx = do
  b <- fetchByteAtPC
  execute b
  fdx

execute :: Word8 -> FDX ()
execute opc = case opc of
  -- ADC, Add Memory to Accumulator with Carry
  -- A + M + C -> A, C
  -- N Z C I D V
  -- + + + - - +
  --
  0x69 -> incACBy fetchByteAtPC
  0x65 -> incACBy $ do
    b <- fetchByteAtPC
    fetchByteMem (fromIntegral b)
  0x75 -> incACBy $ do
    b <- fetchByteAtPC
    x <- getX
    fetchByteMem (fromIntegral (b + x)) -- zeropage
  0x6D -> incACBy (fetchWordAtPC >>= fetchByteMem)
  0x7D -> incACBy $ do
    w <- fetchWordAtPC
    x <- getX
    -- TODO: what does it mean to increment the address with carry?
    -- I think it means that you convert x to 16 bit and then add
    fetchByteMem (w + (fromIntegral x))
  0x79 -> incACBy $ do
    w <- fetchWordAtPC
    y <- getY
    -- TODO: what does it mean to increment the address with carry?
    -- I think it means that you convert y to 16 bit and then add
    fetchByteMem (w + (fromIntegral y))
  0x61 -> incACBy $ do
    b <- fetchByteAtPC
    x <- getX
    fetchByteMem (fromIntegral (b + x)) -- zeropage indexed by x
  0x71 -> incACBy $ do
    -- TODO: I don't understand this one at all...
    b <- fetchByteAtPC
    y <- getY
    -- I think with carry means to promote b,y to Word16 then add
    fetchByteMem (fromIntegral b + fromIntegral y)
  -- TODO: all unimplemented opcodes are nop
  _ -> do
    return ()
