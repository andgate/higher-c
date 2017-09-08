{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Parse.Token
import Language.Hawk.Syntax.Decl
import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data ParseErr
  = UnexpectedToken [Token]
  | AmbiguousGrammar [Decl]
  deriving(Show)

makeClassyPrisms ''ParseErr

instance Pretty ParseErr where
    pretty = \case
      UnexpectedToken ts ->
          P.textStrict "Unexpected tokens:" P.<+> P.dquotes (P.pretty ts)

      AmbiguousGrammar ds ->
          P.textStrict "Severe Parser Error: Ambiguous grammar encountered. Please report."
          P.<$> P.pretty ds
