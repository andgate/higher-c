{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data ParseErr
  = ParseFailed Text
  | UndefinedParseErr
  deriving(Show)

makeClassyPrisms ''ParseErr

instance Pretty ParseErr where
    pretty = \case
      ParseFailed msg ->
          P.textStrict msg

      UndefinedParseErr ->
          P.textStrict "Undefined Parse Error encountered."
