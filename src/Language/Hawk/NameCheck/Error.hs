{-# LANGUAGE  TemplateHaskell, LambdaCase, OverloadedStrings #-}
module Language.Hawk.NameCheck.Error where

import Control.Lens
import Data.Text
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP

data NcErr
  = UndeclaredNameFound Text Loc
  | UnknownNcErr
  deriving(Show)

makeClassyPrisms ''NcErr

instance Pretty NcErr where
    pretty = \case
      UndeclaredNameFound n l ->
        PP.pretty l
          PP.<+> PP.textStrict ": Unknown symbol encountered"
          PP.<+> PP.dquotes (PP.textStrict n)

      UnknownNcErr -> PP.text "unknown namechecker error"
