module Hawk.AST where

import Data.Sequence (Seq)

type Name = String
type Ids  = [String]
type Params = [String]
type ModId = [String]
type Types = [String]
type Exprs = [Expr]

data Expr
  = IntExpr Int
  | FloatExpr Double
  | StringExpr String
  | UnaryOpExpr String Expr
  | BinaryOpExpr String Expr Expr
  | VarExpr String
  
  | ModuleExpr ModId Exprs
  | ImportExpr ModId Bool
  
  | VarDecExpr Name Types Expr
  | ValDecExpr Name Types Expr
  
  | FuncExpr Name Params Types Expr
  | ExternExpr Name Params Types
  
  | LetExpr Name Expr Expr
  | CallExpr Name Exprs
  
  | DoExpr Exprs
  | ReturnExpr Expr
  | IfExpr Expr Expr Expr
  | WhileExpr Expr Expr
  deriving (Eq, Show)
  
  
mkFuncExpr :: (Name, Params, Types) -> Expr -> Expr
mkFuncExpr (n, p, t) b = FuncExpr n p t b

mkExternExpr :: (Name, Params, Types) -> Expr
mkExternExpr (n, p, t) = ExternExpr n p t