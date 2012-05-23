{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Emulator.Machine
import Emulator.Memory
import Emulator.Registers
import Emulator.Instructions

import Data.Vector.Unboxed.Mutable

import Prelude hiding ( catch )
import Control.Exception

main :: IO ()
main = do
  let rs  = mkRegisters { rAC = 2 }
  mem <- memory
  write mem 0 0x69
  write mem 1 40
  let machine = Machine rs mem
      step :: FDX IO ()
      step = do
        b <- fetchByteAtPC
        execute b
      update m = do
        t <- try $ runMachine step m
        case t of
          Right (_,m') -> update m'
          Left (e::SomeException) -> print e >> return (mRegs m)
  !m <- update machine
  print m
