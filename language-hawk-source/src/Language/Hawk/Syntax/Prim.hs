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
{-
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
-}

data PrimVal
  = VInt Integer
  | VFloat Double
  | VChar Char
  | VBool Bool
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)


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


instance Pretty PrimVal where
  pretty = \case
    VInt v ->
      pretty v
        
    VFloat v ->
      pretty v
    
    VChar c ->
      squotes $ pretty c
    
    VBool v ->
      pretty v


instance Pretty PrimInstr where
    pretty =
      pretty . pack . show


-- Serialize ---------------------------------------------------------------------

instance Binary PrimVal
instance FromJSON PrimVal
instance ToJSON PrimVal

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




solve :: (PrimInstr, PrimVal, PrimVal) -> PrimVal
solve = \case
  (PrimAdd, VInt x, VInt y) -> VInt (x + y)
  (PrimFAdd, VFloat x, VFloat y) -> VFloat (x + y)
  (PrimSub, VInt x, VInt y) -> VInt (x - y)
  (PrimFSub, VFloat x, VFloat y) -> VFloat (x - y)
  (PrimMul, VInt x, VInt y) -> VInt (x * y)
  (PrimFMul, VFloat x, VFloat y) -> VFloat (x * y)
  (PrimDiv, VInt x, VInt y) -> VInt (div x y)
  (PrimFDiv, VFloat x, VFloat y) -> VFloat (x / y)
  (PrimEq, VInt x, VInt y) -> VBool (x == y)
  (PrimLt, VInt x, VInt y) -> VBool (x <= y)
  (PrimLtEq, VInt x, VInt y) -> VBool (x <= y)
  (PrimGt, VInt x, VInt y) -> VBool (x > y)
  (PrimtGtEq, VInt x, VInt y) -> VBool (x >= y)
  (PrimNEq, VInt x, VInt y) -> VBool (not $ x == y)
  (PrimNLt, VInt x, VInt y) -> VBool (not $ x < y)
  (PrimNLtEq, VInt x, VInt y) -> VBool (not $ x <= y)
  (PrimNGt, VInt x, VInt y) -> VBool (not $ x > y)
  (PrimNGtEq, VInt x, VInt y) -> VBool (not $ x >= y)
  _ -> error "Unsupported operation"