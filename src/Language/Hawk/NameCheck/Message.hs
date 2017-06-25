{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.NameCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data NameCheckMessage
  = NcSuccess
  | NcFailed
  deriving(Show)

makeClassyPrisms ''NameCheckMessage

instance Pretty NameCheckMessage where
    pretty msg =
      undefined