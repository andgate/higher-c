{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Parse.Message where

import Control.Lens

data ParseMessage
  = ParseSuccess
  | ParseFailed

makeClassyPrisms ''ParseMessage