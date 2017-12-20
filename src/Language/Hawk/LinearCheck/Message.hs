{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP


data LcMsg
  = LcBegin
  | LcComplete
  | LcUndefined
  deriving(Show)

makeClassyPrisms ''LcMsg

instance Pretty LcMsg where
    pretty = \case
      LcBegin ->
        PP.textStrict "Linearity check has begun."

      LcComplete ->
        PP.textStrict "Linearity check completed."

      LcUndefined ->
        PP.textStrict "Linearity checker encountered an undefined error."
