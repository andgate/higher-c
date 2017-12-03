{-# LANGUAGE  GeneralizedNewtypeDeriving
            , TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Target.LLVM.Codegen where

import Control.Lens
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Language.Hawk.CPS.Syntax
import Language.Hawk.Target.LLVM.Instruction
import Language.Hawk.Target.LLVM.IR
import Language.Hawk.Target.LLVM.Module
import LLVM.Pretty

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST

import qualified Data.ByteString.Short  as BS
import qualified LLVM.AST.Float         as F
import qualified LLVM.AST.Constant      as C
import qualified Data.Text.Encoding     as T


codegen :: Program -> Module
codegen (Program ds) =
  buildModule "example" [] $
    mapM codegenDef ds


codegenDef
  :: (MonadModuleBuilder m)
  => Def -> m Operand
codegenDef (Def n e) =
  codegenComplex e



codegenComplex :: CExp -> m Operand
codegenComplex = \case
  CLet n b e -> undefined
  CIf p a b -> undefined
  CJump a b -> undefined
  CHalt a -> undefined
  

codegenAtomic :: AExp -> m Operand
codegenAtomic = \case
  CLit l -> undefined
  CVar n -> undefined
  CLam n e -> undefined
