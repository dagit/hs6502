module Emulator.Registers where

import Data.Word
import Data.Bits
import Data.Maybe ( catMaybes )

-- | http://www.masswerk.at/6502/6502_instruction_set.html
data Registers = Registers
  { rPC :: !Word16 -- ^ Program Counter
  , rAC :: !Word8  -- ^ Accumulator
  , rX  :: !Word8  -- ^ X register
  , rY  :: !Word8  -- ^ Y register
  , rSR :: !Word8  -- ^ Status register [NV-BDIZC]
  , rSP :: !Word8  -- ^ Stack pointer
  } deriving (Read, Show, Eq, Ord)

data SRFlag = Carry      -- ^ bit 0
            | Zero       -- ^ bit 1
            | Interrupt  -- ^ bit 2
            | Decimal    -- ^ bit 3
            | Break      -- ^ bit 4
            | Ignored    -- ^ bit 5
            | Overflow   -- ^ bit 6
            | Negative   -- ^ bit 7
  deriving (Read, Show, Eq, Ord, Enum)

-- | Construct the registers all with initial value of 0
mkRegisters :: Registers
mkRegisters = Registers
  { rPC = 0
  , rAC = 0
  , rX  = 0
  , rY  = 0
  , rSR = 0
  , rSP = 0
  }

-- | all the status register flags in a handy list
allSRFlags = [Carry .. Negative]

-- | Look up the value of a particular flag by name.
-- Nothing means the flag is not set, Just flag means it
-- is set.
lookupSRFlag :: Registers -> SRFlag -> Maybe SRFlag
lookupSRFlag (Registers { rSR = sr }) f
  | testBit sr (fromEnum f) = Just f
  | otherwise               = Nothing

-- | This returns all the SRFlags that are currently set,
-- the return type is morally `Data.Set.Set SRFlag`
getSRFlags :: Registers -> [SRFlag]
getSRFlags rs =
  catMaybes (zipWith lookupSRFlag (repeat rs) allSRFlags)

-- | Applies a bit transformation at the specified status register bit
atSRFlag :: (Word8 -> Int -> Word8) -> Registers -> SRFlag -> Registers
atSRFlag f rs@(Registers { rSR = sr }) flag =
  rs { rSR = f sr (fromEnum flag) }

-- | Clears a specific status register flag
clearSRFlag :: Registers -> SRFlag -> Registers
clearSRFlag = atSRFlag clearBit

-- | Sets a specific status register flag
setSRFlag :: Registers -> SRFlag -> Registers
setSRFlag = atSRFlag setBit

-- | Complements a specific status register flag
complementSRFlag :: Registers -> SRFlag -> Registers
complementSRFlag = atSRFlag complementBit

-----------------------------------------------------------------
-- With the exception of clearSRFlags these are overkill

-- | Applies a function at every bit
atSRFlags :: (Word8 -> Int -> Word8) -> Registers -> Registers
atSRFlags f rs@(Registers { rSR = sr }) =
  rs { rSR = foldl f sr (map fromEnum allSRFlags) }

-- | Clears all the bits in the status register.
clearSRFlags :: Registers -> Registers
clearSRFlags rs = rs { rSR = 0 }

-- | Sets every bit in the status register
setSRFlags :: Registers -> Registers
setSRFlags = atSRFlags setBit

-- | Compelement every bit in the status register
complementSRFlags :: Registers -> Registers
complementSRFlags = atSRFlags complementBit
