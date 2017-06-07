module Language.Hawk.Syntax.Prim where

import Language.Hawk.Syntax.Literal
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Text.PrettyPrint.ANSI.Leijen as PP


data PrimOperand n
  = NamedOperand n
  | LiteralOperand Literal
  deriving (Show, Eq, Ord)


-- Hawk digs it's talon's into LLVM's instruction set.
data PrimOp n
  = Add (PrimOperand n) (PrimOperand n)
  | FAdd (PrimOperand n) (PrimOperand n)
  | Sub (PrimOperand n) (PrimOperand n)
  | FSub (PrimOperand n) (PrimOperand n)
  | Mul (PrimOperand n) (PrimOperand n)
  | FMul (PrimOperand n) (PrimOperand n)
  | Div (PrimOperand n) (PrimOperand n)
  | UDiv (PrimOperand n) (PrimOperand n)
  | SDiv (PrimOperand n) (PrimOperand n)
  | FDiv (PrimOperand n) (PrimOperand n)
  deriving (Show, Eq, Ord)