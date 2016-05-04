module Language.Hawk.Analysis.Rename.RnMonad where

import Language.Hawk.Syntax.AST
import Control.Monad.State.Lazy


type RnM a = TcRn a
type TcM a = TcRn a
type TcRn a = TcRnIf RnEnv a
type TcRnIf a e = StateT e IO a

data RnEnv
  = RnEnv
  { env_ast :: AstNode
  , env_gbl :: SymbolTable
  , env_lcl :: SymbolTable
  }