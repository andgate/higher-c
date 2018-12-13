{-# LANGUAGE LambdaCase #-}
module Language.HigherC.Syntax.Core where

import Data.Text (Text, unpack)

import Language.HigherC.Syntax.Builtin
import Language.HigherC.Syntax.Location

data Name
  = Name Loc Text
  | Builtin  Text

nameText :: Name -> Text
nameText = \case
  Name _  txt -> txt
  Builtin txt -> txt

nameStr :: Name -> String
nameStr = \case
  Name _  txt -> unpack txt
  Builtin txt -> unpack txt

data Object
  = Object Name [Defn]

data Defn
  = DGlobal  GlobalDecl
  | DFunc    FuncDefn
  | DExtern  ExternDefn

data GlobalDecl
  = GlobalDecl Name Type ConstExp

data FuncDefn
  = FuncDefn FuncDecl [Statement]

data FuncDecl
  = FuncDecl Name [Parameter] Type

data ExternDefn
  = ExternDefn Name [Parameter] Type

data Parameter
  = Parameter Name Type

data Statement
  = SNop
  | SExp Exp

data Exp
  = EVar Name
  | EVal Val

data ConstExp
  = CEInt Integer

data Type
  = TCon Name
  | TPtr Type
  | TArray Type
  | TArraySized Type Integer

data TypeDefn
  = TypeDefn Name [Type]
