{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase 
  #-}
module Language.Hawk.SubtermCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP

data StcMsg
  = StcBegin
  | StcComplete
  deriving(Show)

makeClassyPrisms ''StcMsg

instance Pretty StcMsg where
    pretty = \case
      StcBegin ->
        PP.textStrict "Subtype checking has begun."

      StcComplete ->
        PP.textStrict "Subtype checking completed."