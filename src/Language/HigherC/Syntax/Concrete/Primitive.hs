{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
module Language.HigherC.Syntax.Concrete.Primitive where

import Control.Lens.Plated
import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.HigherC.Syntax.Location


-- -----------------------------------------------------------------------------
-- | Type

data Value e
  = VNull
  | VBool Bool           -- supports 1 bit integers
  | VInt Integer -- supports 1, 8, 16, 32, 64, and 128 bit integers
  | VFp Double   -- supports 16, 32, 64, and 128 bit floats
  | VChar Char     -- supports 8 bit chars
  | VArray [e] -- Can contain a list of other values
  | VVector [e] -- SIMD optimized list of integers or floats
  | VString [Char] -- Only store a string as an 8-bit char
  deriving (Show, Generic, Data, Typeable)


data TypeCon t e
  = TVoid Loc
  | TInt Loc Integer
  | TFp  Loc Integer
  | TArray Loc t
  | TArraySized Loc t e
  | TVector Loc t
  | TVectorSized Loc t e
  | TPtr   Loc t
  | TRef   Loc t
  | TRVal  Loc t
  | TConst Loc t
  deriving (Show, Generic, Data, Typeable)


data Instruction a
  = IAdd a a
  | ISub a a
  | IMul a a
  | IDiv a a
  | UDiv a a
  | SDiv a a

  -- Floating Point Math
  | FAdd a a
  | FSub a a
  | FMul a a
  | FDiv a a

  -- Comparisions
  | Eq a a
  | Lt a a
  | LtEq a a
  | Gt a a
  | GtEq a a
  | NEq a a
  | NLt a a
  | NLtEq a a
  | NGt a a
  | NGtEq a a
  deriving (Show, Generic, Data, Typeable)


intInstrs :: a -> a -> [Instruction a]
intInstrs a b =
  [ IAdd a b
  , ISub a b
  , IMul a b
  , IDiv a b
  , UDiv a b
  , SDiv a b
  ]


floatInstrs :: a -> a -> [Instruction a]
floatInstrs a b =
  [ FAdd a b
  , FSub a b
  , FMul a b
  , FDiv a b
  , FDiv a b
  ]


-- -----------------------------------------------------------------------------
-- | Instances

-- Location ---------------------------------------------------------------------

instance (Locatable t, Locatable e) => Locatable (TypeCon t e) where
  locOf = \case
    TVoid l -> l
    TInt l _ -> l
    TFp l _ -> l
    TArray l _ -> l
    TArraySized l _ _ -> l
    TVector l _ -> l
    TVectorSized l _ _ -> l
    TPtr l _ -> l
    TRef l _ -> l
    TRVal l _ -> l
    TConst l _ -> l


-- Plated ---------------------------------------------------------------------

instance (Data e) => Plated (Value e)
instance (Data t, Data e) => Plated (TypeCon t e)
instance (Data e) => Plated (Instruction e)

-- Pretty ---------------------------------------------------------------------

instance Pretty e => Pretty (Value e) where
  pretty = \case
    VNull -> "null"
    VBool b -> pretty b
    VInt i -> pretty i
    VFp d -> pretty d
    VChar c -> pretty c
    VArray vs -> encloseSep "[" "]" "," (pretty <$> vs)
    VVector vs ->  encloseSep "<." ">" "," (pretty <$> vs)
    VString cs -> pretty cs


instance (Pretty t, Pretty e) => Pretty (TypeCon t e) where
  pretty = \case
    TVoid _ -> "Void"
    TInt _ bits -> "I"  <> pretty bits
    TFp  _ bits -> "Fp" <> pretty bits

    TArray _ t        -> pretty t <> "[]"
    TArraySized _ t e -> pretty t <> "[" <> pretty e <> "]"

    TVector _ t        -> pretty t <> "<>"
    TVectorSized _ t e -> pretty t <> "<" <> pretty e <> ">"

    TPtr   _ t ->     "*" <>  pretty t
    TRef   _ t ->     "&" <>  pretty t
    TRVal  _ t ->    "&&" <>  pretty t
    TConst _ t -> "const" <+> pretty t


instance (Pretty a) => Pretty (Instruction a) where
    pretty = \case
      IAdd a b -> "__add" <> tupled [pretty a, pretty b]
      ISub a b -> "__sub" <> tupled [pretty a, pretty b]
      IMul a b -> "__mul" <> tupled [pretty a, pretty b]
      IDiv a b -> "__div" <> tupled [pretty a, pretty b]
      UDiv a b -> "__udiv" <> tupled [pretty a, pretty b]
      SDiv a b -> "__sdiv" <> tupled [pretty a, pretty b]

      FAdd a b -> "__fadd" <> tupled [pretty a, pretty b]
      FSub a b -> "__fsub" <> tupled [pretty a, pretty b]
      FMul a b -> "__fmul" <> tupled [pretty a, pretty b]
      FDiv a b -> "__fdiv" <> tupled [pretty a, pretty b]

      Eq a b -> "__eq" <> tupled [pretty a, pretty b]
      Lt a b -> "__lt" <> tupled [pretty a, pretty b]
      LtEq a b -> "__lteq" <> tupled [pretty a, pretty b]
      Gt a b -> "__gt" <> tupled [pretty a, pretty b]
      GtEq a b -> "__gteq" <> tupled [pretty a, pretty b]
      NEq a b -> "__neq" <> tupled [pretty a, pretty b]
      NLt a b -> "__nlt" <> tupled [pretty a, pretty b]
      NLtEq a b -> "__nlteq" <> tupled [pretty a, pretty b]
      NGt a b -> "__ngt" <> tupled [pretty a, pretty b]
      NGtEq a b -> "__ngteq" <> tupled [pretty a, pretty b]

-- For parsing
readPrimInstr :: a -> a -> Text -> Instruction a
readPrimInstr a b = \case
  "__add"   -> IAdd a b
  "__fadd"  -> FAdd a b
  "__sub"   -> ISub a b
  "__fsub"  -> FSub a b
  "__mul"   -> IMul a b
  "__fmul"  -> FMul a b
  "__div"   -> IDiv a b
  "__udiv"  -> UDiv a b
  "__sdiv"  -> SDiv a b
  "__fdiv"  -> FDiv a b
  "__eq"    -> Eq a b
  "__lt"    -> Lt a b
  "__lteq"  -> LtEq a b
  "__gt"    -> Gt a b
  "__gteq"  -> GtEq a b
  "__neq"   -> NEq a b
  "__nlt"   -> NLt a b
  "__nlteq" -> NLtEq a b
  "__ngt"   -> NGt a b
  "__ngteq" -> NGtEq a b



{-
evalInstr :: (Instr, Val, Val) -> Val
evalInstr = \case
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
-}
