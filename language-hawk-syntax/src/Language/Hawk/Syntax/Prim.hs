{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
module Language.Hawk.Syntax.Prim where

import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)


-- -----------------------------------------------------------------------------
-- | Type

-- Primitive constructor.
-- The language makes each instance a monad.
-- Each constructor bends the language rules a little bit. 
data PrimCon
  = Cell
  | Box
  | Linear
  | Ref
  | Safe
  | Dynamic
  | Lazy
  | GC
  | IO
  deriving (Read, Show, Eq, Ord, Enum, Data, Typeable, Generic)


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

instance Pretty PrimCon where
    pretty =
      pretty . pack . show

instance Pretty PrimInstr where
    pretty =
      pretty . pack . show


-- Serialize ---------------------------------------------------------------------

instance Binary PrimCon
instance FromJSON PrimCon
instance ToJSON PrimCon

instance Binary PrimInstr
instance FromJSON PrimInstr
instance ToJSON PrimInstr


-- String

readPrimInstr :: Text -> PrimInstr
readPrimInstr = \case
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
