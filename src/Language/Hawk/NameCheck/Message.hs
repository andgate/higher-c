{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.NameCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data NcMsg
  = NcSuccess
  | NcFailed
  deriving(Show)

makeClassyPrisms ''NcMsg

instance Pretty NcMsg where
    pretty msg =
      undefined
