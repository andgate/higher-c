{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.KindsCheck.Error where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Type (Type)
import Language.Hawk.Syntax.Kind (Kind)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data KcErr
  = KindUnknown Text
  | KindMismatch Kind Kind
  | KindArrowExpected Kind
  deriving(Show)

makeClassyPrisms ''KcErr

instance Pretty KcErr where
    pretty = \case
      KindUnknown n ->
        undefined

      KindMismatch expected actual ->
        undefined
