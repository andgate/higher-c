{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax (ItemPs)

data ParseErr
  = UnexcpectedToken Token
  | ParseFail Token
  | AmbiguousGrammar [[ItemPs]]
  deriving(Show)

makeClassyPrisms ''ParseErr