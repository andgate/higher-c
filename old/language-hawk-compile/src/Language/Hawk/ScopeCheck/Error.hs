{-# LANGUAGE  TemplateHaskell, LambdaCase, OverloadedStrings #-}
module Language.Hawk.ScopeCheck.Error where

import Control.Lens
import Data.Text
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP

data ScErr
  = ScUndeclared Text Loc
  | ScUnknownErr
  deriving(Show)

makeClassyPrisms ''ScErr

instance Pretty ScErr where
    pretty = \case
      ScUndeclared n l ->
        PP.pretty l
          PP.<> PP.textStrict ": Scope checker encountered unknown symbol"
          PP.<+> PP.dquotes (PP.textStrict n)

      ScUnknownErr -> PP.text "Unknown scope checker error."
