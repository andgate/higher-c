{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data PsErr
  = UnexpectedToken [Token]
  | AmbiguousGrammar
  deriving(Show)

makeClassyPrisms ''PsErr

instance Pretty PsErr where
    pretty = \case
      UnexpectedToken ts ->
          P.textStrict "Unexpected tokens:" P.<+> P.dquotes (P.pretty ts)

      AmbiguousGrammar ->
          P.textStrict "Severe Parser Error: Ambiguous grammar encountered. Please report."
