{-# Language  LambdaCase #-}
module Language.Hawk.Target.LLVM.Generate where


import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import qualified Language.Hawk.Syntax            as Hk
import qualified Language.Hawk.Syntax.Term.Basic as Basic


codegen :: [(String, Basic.Term ())] -> Module
codegen fns = buildModule "someModule" $ mapM cgTerm fns


cgFunc :: (String, Basic.Term ()) -> ModuleBuilder ()
cgFunc (n, t) = \case
  _ -> undefined

cgTerm :: (String, Basic.Term ()) -> ModuleBuilder ()
cgTerm = \case
  _ -> undefined