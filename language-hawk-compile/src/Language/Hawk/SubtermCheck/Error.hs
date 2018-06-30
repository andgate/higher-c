{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.SubtermCheck.Error where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Term.Scoped
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data StcErr
  = StcUnknownError Text
  | StcMismatch (Term ()) (Term ())
  | StcArrowExpected (Term ())

makeClassyPrisms ''StcErr

instance Pretty StcErr where
    pretty = \case
      StcUnknownError n ->
        undefined

      StcMismatch expected actual ->
        undefined
