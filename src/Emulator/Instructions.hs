{-# LANGUAGE BangPatterns #-}
module Emulator.Instructions where

import Emulator.Memory
import Emulator.Registers
import Emulator.Machine

import Data.Word
import Data.Bits

getReg :: Monad m => (Registers -> a) -> FDX m a
getReg f = do
  rs <- getRegisters
  return $! f rs

getPC :: Monad m => FDX m Word16
getPC = getReg rPC

setPC :: Monad m => Word16 -> FDX m ()
setPC !pc = do
  rs <- getRegisters
  setRegisters $! rs { rPC = pc }
  
incPC :: Monad m => FDX m ()
incPC = do
  pc <- getPC
  setPC $! 1+pc

getAC :: Monad m => FDX m Word8
getAC = getReg rAC

setAC :: Monad m => Word8 -> FDX m ()
setAC !w = do
  rs <- getRegisters
  setRegisters $! rs { rAC = w }

getX :: Monad m => FDX m Word8
getX = getReg rX

getY :: Monad m => FDX m Word8
getY = getReg rY

fetchByteAtPC :: FDX IO Word8
fetchByteAtPC = do
  pc  <- getPC
  mem <- getMemory
  incPC
  b <- fetchByte pc mem
  return b

fetchWordAtPC :: FDX IO Word16
fetchWordAtPC = do
  low  <- fetchByteAtPC
  high <- fetchByteAtPC
  return $! mkWord low high

fetchBytesAtPC :: Int -> FDX IO [Word8]
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

fdx :: FDX IO ()
fdx = do
  b <- fetchByteAtPC
  execute b
  fdx

execute :: Word8 -> FDX IO ()
execute opc = case opc of
  -- ADC, Add Memory to Accumulator with Carry
  -- A + M + C -> A, C
  -- N Z C I D V
  -- + + + - - +
  --
  0x69 -> do
    m  <- fetchByteAtPC
    ac <- getAC
    -- TODO: update status register
    setAC $! ac + m
  0x65 -> do
    b   <- fetchByteAtPC
    mem <- getMemory
    ac  <- getAC
    m   <- fetchByte (fromIntegral b) mem -- zeropage
    setAC $! ac + m
  0x75 -> do
    b   <- fetchByteAtPC
    x   <- getX
    mem <- getMemory
    ac  <- getAC
    m   <- fetchByte (fromIntegral (b + x)) mem -- zeropage
    setAC $! ac + m
  0x6D -> do
    w   <- fetchWordAtPC
    mem <- getMemory
    ac  <- getAC
    m   <- fetchByte w mem
    setAC $! ac + m
  0x7D -> do
    w   <- fetchWordAtPC
    mem <- getMemory
    x   <- getX
    ac  <- getAC
    -- TODO: what does it mean to increment the address with carry?
    m   <- fetchByte (w + (fromIntegral x)) mem
    setAC $! ac + m
  0x79 -> do
    w   <- fetchWordAtPC
    mem <- getMemory
    y   <- getY
    ac  <- getAC
    -- TODO: what does it mean to increment the address with carry?
    -- I think it means that you convert y to 16 bit and then add
    m   <- fetchByte (w + (fromIntegral y)) mem
    setAC $! ac + m
  0x61 -> do
    b   <- fetchByteAtPC
    mem <- getMemory
    x   <- getX
    ac  <- getAC
    m   <- fetchByte (fromIntegral (b + x)) mem -- zeropage indexed by x
    setAC $! ac + m
  0x71 -> do
  -- TODO: I don't understand this one at all...
    b   <- fetchByteAtPC
    mem <- getMemory
    y   <- getY
    ac  <- getAC
    -- I think with carry means to promote b,y to Word16 then add
    m   <- fetchByte (fromIntegral b + fromIntegral y) mem
    setAC $! ac + m
  -- TODO: all unimplemented opcodes are nop
  _ -> do
    return ()
