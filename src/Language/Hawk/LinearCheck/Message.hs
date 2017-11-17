{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP


data LcMsg
  = LcComplete
  | LcUndefined
  deriving(Show)

makeClassyPrisms ''LcMsg

instance Pretty LcMsg where
    pretty = \case
      LcComplete ->
        PP.textStrict "Linearity check completed."

      LcUndefined ->
        PP.textStrict "Linearity checker encountered an undefined error."
