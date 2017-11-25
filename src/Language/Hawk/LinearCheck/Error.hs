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

import qualified Text.PrettyPrint.Leijen.Text as PP


data LcErr
  = LcPreviouslyConsumed Text
  | LcLamUnconsumed Text
  | LcLetUnconsumed Text
  | LcBranchMismatch [Text]
  | LcUnknownErr
  deriving(Show)

makeClassyPrisms ''LcErr

instance Pretty LcErr where
    pretty = \case
      LcPreviouslyConsumed n ->
        PP.textStrict "Linear variable"
          PP.<+> PP.squotes (PP.textStrict n)
          PP.<+> "was already consumed."

      LcLamUnconsumed n ->
        PP.textStrict "Unconsumed variable"
          PP.<+> PP.squotes (PP.textStrict n)
          PP.<+> PP.textStrict "introduced by lambda expression."

      LcLetUnconsumed n ->
        PP.textStrict "Unconsumed variable"
          PP.<+> PP.squotes (PP.textStrict n)
          PP.<+> PP.textStrict "introduced by let expression."

      LcBranchMismatch ns ->
        PP.textStrict "Mismatch in linear variable consumption between branches in if expression. Variables are"
          PP.<+> PP.vcat (map PP.pretty ns)
          PP.<+> PP.textStrict "introduced by let expression."

      LcUnknownErr ->
        PP.textStrict "Unbound variable encountered:"
