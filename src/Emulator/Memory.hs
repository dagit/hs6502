{-# LANGUAGE FlexibleContexts #-}
module Emulator.Memory where

import Prelude hiding ( replicate, read )
import Data.Vector.Unboxed.Mutable
import Data.Word

import MonadLib

type Memory = IOVector Word8

addressSize :: Int
addressSize = 2^(16::Int)

memory :: IO Memory
memory = replicate addressSize 0

fetchByte :: BaseM m IO => Word16 -> Memory -> m Word8
fetchByte addr m = inBase (read m (fromIntegral addr))

writeByte :: BaseM m IO => Word16 -> Word8 -> Memory -> m ()
writeByte addr b m = inBase (write m (fromIntegral addr) b)
