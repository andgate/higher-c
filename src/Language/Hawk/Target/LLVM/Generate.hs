{-# Language  LambdaCase #-}
module Language.Hawk.Target.LLVM.Generate where

import Language.Hawk.Target.LLVM.Instruction
import Language.Hawk.Target.LLVM.IR
import Language.Hawk.Target.LLVM.Module

import qualified Language.Hawk.Syntax            as Hk
import qualified Language.Hawk.Syntax.Term.Basic as Hk


generateTerm :: Basic.Term () -> LLVM.Module
generateTerm = \case
  _ -> undefined