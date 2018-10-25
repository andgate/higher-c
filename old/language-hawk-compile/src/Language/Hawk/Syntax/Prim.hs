{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
module Language.Hawk.Syntax.Prim where

import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Text.PrettyPrint.Leijen.Text as PP


-- -----------------------------------------------------------------------------
-- | Type

-- Hawk talon's are dug firmly into LLVM's instruction set.
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
  | PrimEq
  | PrimLt
  | PrimLtEq
  | PrimGt
  | PrimtGtEq
  | PrimNEq
  | PrimNLt
  | PrimNLtEq
  | PrimNGt
  | PrimNGtEq
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




-- Serialize ---------------------------------------------------------------------

instance Binary PrimInstr
instance FromJSON PrimInstr
instance ToJSON PrimInstr

-- String

readPrim :: Text -> PrimInstr
readPrim = \case
  "#add"   -> PrimAdd
  "#fadd"  -> PrimFAdd
  "#sub"   -> PrimSub
  "#fsub"  -> PrimFSub
  "#mul"   -> PrimMul
  "#fmul"  -> PrimFMul
  "#div"   -> PrimDiv
  "#udiv"  -> PrimUDiv
  "#sdiv"  -> PrimSDiv
  "#fdiv"  -> PrimFDiv
  "#eq"    -> PrimEq
  "#lt"    -> PrimLt
  "#lteq"  -> PrimLtEq
  "#gt"    -> PrimGt
  "#gteq"  -> PrimtGtEq
  "#neq"   -> PrimNEq
  "#nlt"   -> PrimNLt
  "#nlteq" -> PrimNLtEq
  "#ngt"   -> PrimNGt
  "#ngteq" -> PrimNGtEq

   -- What was this for?
  
  _ -> PrimBad
