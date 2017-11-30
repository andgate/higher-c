{-# LANGUAGE OverloadedStrings, RecursiveDo, LambdaCase #-}
module Language.Hawk.Target.LLVM where

import Data.Text (Text)
import Language.Hawk.Syntax
import Language.Hawk.Target.LLVM.Instruction
import Language.Hawk.Target.LLVM.IR
import Language.Hawk.Target.LLVM.Module
import LLVM.Pretty

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C


codegen :: Image -> Module
codegen r =
  buildModule "example" [] $
    function
      "add"
      [ (i32, Just "a")
      , (i32, Just "b") ]
      i32
      $ \[a,b] -> do
          c <- add a b
          ret c


codegenExp :: Text -> Exp -> ModuleBuilder a
codegenExp = \case
  e -> undefined
