{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Language.Hawk.Syntax.Type (Type)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data TcErr
  = TypeMismatch String String
  | UnboundVariable String
  | FoundBox
  deriving(Show)

makeClassyPrisms ''TcErr

instance Pretty TcErr where
    pretty = \case
      TypeMismatch have want ->
        P.textStrict "Type mismatch:"
        P.<$>
        P.indent 2 
        ( P.textStrict "actual:" P.<+> pretty have
          P.<$>
          P.textStrict "expected" P.<+> pretty want
        )

      UnboundVariable v ->
        P.textStrict "Unbound variable encountered:" P.<+> pretty v
      
      FoundBox ->
        P.textStrict "Encountered Box"
