{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.TypeCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P

data TcMsg
  = TcBegin
  | TcComplete
  | TcUndefined
  deriving(Show)

makeClassyPrisms ''TcMsg

instance Pretty TcMsg where
    pretty = \case
      TcBegin ->
        P.textStrict "Typechecking has began."

      TcComplete ->
        P.textStrict "Typechecker has completed."

      TcUndefined ->
        undefined
