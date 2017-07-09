{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax (Item)
import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data ParseErr
  = UnexpectedToken Token
  | UnexpectedParseErr FilePath
  | UndefinedParseErr
  deriving(Show)

makeClassyPrisms ''ParseErr

instance Pretty ParseErr where
    pretty = \case
      UnexpectedToken t  ->
          P.textStrict "Unexpected token"
            <+> P.squotes (P.textStrict $ t^.tokText)
            <+> P.textStrict "at" <+> P.pretty (t^.tokLoc)

      UndefinedParseErr ->
          undefined