{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase 
  #-}
module Language.Hawk.KindsCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP

data KcMsg
  = KcComplete
  | KcUndefined
  deriving(Show)

makeClassyPrisms ''KcMsg

instance Pretty KcMsg where
    pretty = \case
      KcComplete ->
        PP.textStrict "Kinds checking completed."

      KcUndefined ->
        PP.textStrict "Kinds Checker encountered undefined error."
