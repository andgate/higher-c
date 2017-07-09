{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Language.Hawk.Syntax.Literal where

import Data.Binary
import GHC.Generics (Generic)
import Text.PrettyPrint.Leijen.Text (Pretty (..), squotes)

-- -----------------------------------------------------------------------------
-- | Literal

data Lit
  = IntLit Integer
  | FloatLit Double
  | CharLit Char
  | BoolLit Bool
  deriving (Show, Read, Eq, Ord, Generic)


instance Binary Lit

instance Pretty Lit where
  pretty = \case
    IntLit v ->
      pretty v
        
    FloatLit v ->
      pretty v
    
    CharLit c ->
      squotes $ pretty c
    
    BoolLit v ->
      pretty v