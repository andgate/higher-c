{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
module Language.Hawk.Syntax.Prim where

import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Text.PrettyPrint.Leijen.Text as PP


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
  | PrimBad
  deriving (Read, Show, Eq, Ord, Enum, Data, Typeable, Generic)


intInstrs :: [PrimInstr]
intInstrs = 
  [ PrimAdd
  , PrimSub
  , PrimMul
  , PrimDiv
  , PrimUDiv
  , PrimSDiv
  ]


floatInstrs :: [PrimInstr]
floatInstrs =
  [ PrimFAdd
  , PrimFSub
  , PrimFMul
  , PrimFDiv
  , PrimFDiv
  ]


-- -----------------------------------------------------------------------------
-- | Instances

-- Pretty ---------------------------------------------------------------------


instance PP.Pretty PrimInstr where
    pretty =
      PP.textStrict . pack . show




-- Binary ---------------------------------------------------------------------

instance Binary PrimInstr where
  get =
    toEnum . fromIntegral <$> getWord8
      
  put =
    putWord8 . fromIntegral . fromEnum


-- String

readPrim :: Text -> PrimInstr
readPrim = \case
  "#add"  -> PrimAdd
  "#fadd" -> PrimFAdd
  "#sub"  -> PrimSub
  "#fsub" -> PrimFSub
  "#mul"  -> PrimMul
  "#fmul" -> PrimFMul
  "#div"  -> PrimDiv
  "#udiv" -> PrimUDiv
  "#sdiv" -> PrimSDiv
  "#fdiv" -> PrimFDiv
  
  _ -> PrimBad
