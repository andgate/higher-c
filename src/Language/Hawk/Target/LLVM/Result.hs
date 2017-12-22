{-# LANGUAGE  OverloadedStrings, LambdaCase  #-}
module Language.Hawk.Target.LLVM.Result where

import Control.Lens

import LLVM.AST


data Result = Result
  { _cgmod :: Module
  }
