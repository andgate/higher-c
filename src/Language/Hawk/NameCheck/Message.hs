{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.NameCheck.Message where

import Control.Lens

data NameCheckMessage
  = NcSuccess
  | NcFailed
  deriving(Show)

makeClassyPrisms ''NameCheckMessage