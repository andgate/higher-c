module Language.Hawk.Syntax.Prim where

import Data.Binary
import Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP


-- -----------------------------------------------------------------------------
-- | Type

-- Hawk digs it's talon's into LLVM's instruction set.
data PrimInstr
  = PrimAdd
  | PrimFAdd
  | PrimSub
  | PrimFSub
  | PrimMul
  | PrimFMul
  | PrimDiv
  | PrimUDiv
  | PrimSDiv
  | PrimFDiv
  deriving (Show, Eq, Ord, Enum)


-- -----------------------------------------------------------------------------
-- | Instances

-- Pretty ---------------------------------------------------------------------


instance PP.Pretty PrimInstr where
    pretty =
      PP.text . show


-- Binary ---------------------------------------------------------------------

instance Binary PrimInstr where
  get =
    toEnum . fromIntegral <$> getWord8
      
  put =
    putWord8 . fromIntegral . fromEnum