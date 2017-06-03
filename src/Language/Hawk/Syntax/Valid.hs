module Language.Hawk.Syntax.Valid where

import Language.Hawk.Parse.Lexer.Token (Token)
import qualified Language.Hawk.Syntax.Generic as G


type Item
  = G.Item Name Expr Type

type Name
  = G.QName

type Type
  = [Token]

type QType
  = G.QType Type

type Expr
  = G.Expr Name Type

type TypeSig
  = G.TypeSig Name Type

type Var
  = G.Var Name Expr Type

type NewType
  = G.NewType Name Type

type TypeAlias
  = G.TypeAlias Name Type

type TypeClassInst
  = G.TypeClassInst Name Type

type DataType
  = G.DataType Name Type