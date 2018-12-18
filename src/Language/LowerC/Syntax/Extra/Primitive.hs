{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
module Language.LowerC.Syntax.Extra.Primitive where

import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)


-- -----------------------------------------------------------------------------
-- | Type

data Value t
  = VNull t
  | VBool Bool           -- secretly a 1 bit integer
  | VInt Integer Integer -- supports 1, 8, 16, 32, 64, and 128 bit integers
  | VFp Integer Double   -- supports 16, 32, 64, and 128 bit floats
  | VChar Char     -- supports 8 bit chars
  | VArray t [Value t] -- Can contain a list of other values
  | VVector [Value t]  -- Can contain a list of ints or floats
  | VString [Char] -- supposes 8 bit char strings
  | VInstr t (Instruction t) -- Constant expressions just support instructions
  deriving (Show, Generic, Typeable)


data TypeCon t
  = TVoid
  | TInt Integer
  | TFp Integer
  | TArray t
  | TArraySized t (Value t)
  | TVector t
  | TVectorSized t (Value t)
  | TPtr t
  | TRef t
  | TRVal t
  | TConst t
  deriving (Show, Generic, Typeable)


data Instruction t
  -- Integer Math
  = IAdd (Value t) (Value t)
  | ISub (Value t) (Value t)
  | IMul (Value t) (Value t)
  | IDiv (Value t) (Value t)
  | UDiv (Value t) (Value t)
  | SDiv (Value t) (Value t)

  -- Floating Point Math
  | FAdd (Value t) (Value t)
  | FSub (Value t) (Value t)
  | FMul (Value t) (Value t)
  | FDiv (Value t) (Value t)

  -- Comparisions
  | Eq (Value t) (Value t)
  | Lt (Value t) (Value t)
  | LtEq (Value t) (Value t)
  | Gt (Value t) (Value t)
  | GtEq (Value t) (Value t)
  | NEq (Value t) (Value t)
  | NLt (Value t) (Value t)
  | NLtEq (Value t) (Value t)
  | NGt (Value t) (Value t)
  | NGtEq (Value t) (Value t)
  deriving (Show, Generic, Typeable)


-- -----------------------------------------------------------------------------
-- | Instances

-- Pretty ---------------------------------------------------------------------


instance (Pretty t) => Pretty (Value t) where
  pretty = \case
    VNull _ -> "null"
    VBool b -> pretty b
    VInt _ i -> pretty i
    VFp _ d -> pretty d
    VChar c -> pretty c
    VArray _ vs -> hsep (pretty <$> vs)
    VString cs -> pretty cs
    VInstr _ i -> pretty i 


instance Pretty t => Pretty (TypeCon t) where
  pretty = \case
    TVoid -> "void"
    TInt bits -> "I" <> pretty bits
    TFp bits -> "Fp" <> pretty bits

    TArray t        -> pretty t <> "[]"
    TArraySized t e -> pretty t <> "[" <> pretty e <> "]"

    TVector t        -> pretty t <> "<>"
    TVectorSized t e -> pretty t <> "<" <> pretty e <> ">"

    TPtr t -> "*" <> pretty t
    TRef t -> "&" <> pretty t
    TRVal t -> "&&" <> pretty t
    TConst t -> "const" <+> pretty t


instance (Pretty t) => Pretty (Instruction t) where
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
