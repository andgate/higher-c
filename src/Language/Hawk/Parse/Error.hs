{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data ParseErr
  = UnexpectedToken Token
  | FixityTooLow Int
  | FixityTooHigh Int
  | UnexpectedParseErr
  | UndefinedParseErr
  deriving(Show)

makeClassyPrisms ''ParseErr

instance Pretty ParseErr where
    pretty = \case
      UnexpectedToken t  ->
          P.textStrict "Unexpected token"
            <+> P.squotes (P.textStrict $ t^.tokText)
            <+> P.textStrict "at" <+> P.pretty (t^.tokLoc)

      FixityTooLow x  ->
          P.textStrict "Fixity cannot be less than"
            <+> P.pretty x

      FixityTooHigh x  ->
          P.textStrict "Fixity cannot be greater than"
            <+> P.pretty x

      UnexpectedParseErr ->
          P.textStrict "Unexpected parser error encountered."

      UndefinedParseErr ->
          undefined