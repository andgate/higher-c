{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Parse.Lexer.Token

data ParseError
  = UnexcpectedToken Token
  | UnableToParseAT Token

makeClassyPrisms ''ParseError