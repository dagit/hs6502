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
newtype FDX m a = FDX { unFDX :: StateT Machine m a }
  deriving (Functor, Monad, Applicative)

runMachine :: Monad m => FDX m a -> Machine -> m (a, Machine)
runMachine f m = runStateT m (unFDX f)

iso_FDX :: Iso (StateT Machine m) (FDX m)
iso_FDX = Iso FDX unFDX

instance Monad m => StateM (FDX m) Machine where
  get = derive_get iso_FDX
  set = derive_set iso_FDX

instance MonadT FDX where
  lift m = FDX (lift m)

getRegisters :: Monad m => FDX m Registers
getRegisters = do
  m <- get
  return (mRegs m)

setRegisters :: Monad m => Registers -> FDX m ()
setRegisters rs = do
  m <- get
  set ( m { mRegs = rs } )

getMemory :: Monad m => FDX m Memory
getMemory = do
  m <- get
  return (mMem m)

setMemory :: Monad m => Memory -> FDX m ()
setMemory m = do
  mem <- get
  set (mem { mMem = m })

