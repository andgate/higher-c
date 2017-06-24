{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.TypeCheck.Message where

import Control.Lens

data TypeCheckMessage
  = TypeCheckFail
  | TypeCheckSuccess
  deriving(Show)

makeClassyPrisms ''TypeCheckMessage