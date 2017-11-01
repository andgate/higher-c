{-# LANGUAGE  TemplateHaskell, LambdaCase, OverloadedStrings #-}
module Language.Hawk.NameCheck.Error where

import Control.Lens
import Data.Text
import Language.Hawk.Syntax.Expression
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP

data NcErr
  = UndeclaredNameFound Text Exp
  | UnknownNcErr
  deriving(Show)

makeClassyPrisms ''NcErr

instance Pretty NcErr where
    pretty = \case
      UndeclaredNameFound n e -> PP.text "undeclared name found"

      UnknownNcErr -> PP.text "unknown namechecker error"
