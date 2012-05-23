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

fetchByte :: MonadT m => Word16 -> Memory -> m IO Word8
fetchByte addr m = lift (read m (fromIntegral addr))
