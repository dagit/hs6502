{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Emulator.Machine
(Machine(..)
,runMachine
,FDX
,getRegisters
,setRegisters
,getMemory
,setMemory
) where

import Emulator.Memory
import Emulator.Registers
import MonadLib
import MonadLib.Derive

import Control.Applicative

data Machine = Machine
  { mRegs :: !Registers
  , mMem  :: !Memory
  }

-- | FDX is fetch-decode-execute
newtype FDX a = FDX { unFDX :: StateT Machine IO a }
  deriving (Functor, Monad, Applicative)

runMachine :: FDX a -> Machine -> IO (a, Machine)
runMachine f m = runStateT m (unFDX f)

iso_FDX :: Iso (StateT Machine IO) FDX
iso_FDX = Iso FDX unFDX

instance StateM FDX Machine where
  get = derive_get iso_FDX
  set = derive_set iso_FDX

instance BaseM FDX IO where
  inBase = FDX . inBase

getRegisters :: FDX Registers
getRegisters = do
  m <- get
  return (mRegs m)

setRegisters :: Registers -> FDX ()
setRegisters rs = do
  m <- get
  set ( m { mRegs = rs } )

getMemory :: FDX Memory
getMemory = do
  m <- get
  return (mMem m)

setMemory :: Memory -> FDX ()
setMemory m = do
  mem <- get
  set (mem { mMem = m })

