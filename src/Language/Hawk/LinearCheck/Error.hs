{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.Error where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Type (Type)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data LcErr
  = LinearUsedMultiple Text
  | LcUnknownErr
  deriving(Show)

makeClassyPrisms ''LcErr

instance Pretty LcErr where
    pretty = \case
      LinearUsedMultiple n ->
        P.textStrict "Linear variable overused:" P.<+> P.textStrict n

      LcUnknownErr ->
        P.textStrict "Unbound variable encountered:"
