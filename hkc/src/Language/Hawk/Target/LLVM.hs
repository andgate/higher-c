{-# LANGUAGE OverloadedStrings, RecursiveDo, LambdaCase #-}
module Language.Hawk.Target.LLVM where


import Control.Lens
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Term.Basic
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


{--

codegen :: Image -> Module
codegen img =
  buildModule "example" [] $
    mapM codegenFn (img^.imgFns)

{-
    function
      "add"
      [ (i32, Just "a")
      , (i32, Just "b") ]
      i32
      $ \[a,b] -> do
          c <- add a b
          ret c
-}


codegenFn :: (MonadModuleBuilder m)
          => Fn -> m Operand
codegenFn (Fn n _ e) =
  function n' xs y $ \ops ->
    codegenExp ops e >>= ret
  where
    n' = nameText n
    xs = []
    y = i32


codegenExp :: (MonadModuleBuilder m)
           => [Operand] -> Exp -> IRBuilderT m Operand
codegenExp ops = \case
  _ -> error "cannot generate expression"



nameText :: Text -> Name
nameText = Name . t2sbs


t2sbs :: Text -> ShortByteString
t2sbs =  BS.toShort . T.encodeUtf8

--}
