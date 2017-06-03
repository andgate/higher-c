module Language.Hawk.Syntax.Source where

import Language.Hawk.Parse.Lexer.Token (Token)
import qualified Language.Hawk.Syntax.Generic as G


type Item
  = G.Item Name Expr Type

type Literal
  = G.Literal

type Name
  = G.Name

type Type
  = [Token]

type QType
  = G.QType Type

type Expr
  = [Token]

type Stmt
  = G.Stmt Name Expr Type

type TypeSig
  = G.TypeSig Name Type

type Var
  = G.Var Name Expr Type

type Fun
  = G.Fun Name Expr Type

type NewType 
  = G.NewType Name Type

type TypeAlias
  = G.TypeAlias Name Type

type TypeClass
  = G.TypeClass Name Type

type TypeClassInst
  = G.TypeClassInst Name Type

type DataType
  = G.DataType Name Type