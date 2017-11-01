{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Type (Type)
import Language.Hawk.Syntax.Location
import Language.Hawk.TypeCheck.Types
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data TcErr
  = UnificationFail Type Type
  | InfiniteType Text Type
  | UnboundVariable Text
  | AmbiguousType [Constraint]
  | UnificationMismatch [Type] [Type]
  | FoundBox
  deriving(Show)

makeClassyPrisms ''TcErr

instance Pretty TcErr where
    pretty = \case
      UnificationMismatch have want ->
        P.textStrict "Type mismatch:"
        P.<$>
        P.indent 2 
        ( P.textStrict "actual:" P.<+> pretty have
          P.<$>
          P.textStrict "expected" P.<+> pretty want
        )

      UnboundVariable v ->
        P.textStrict "Unbound variable encountered:" P.<+> pretty v
