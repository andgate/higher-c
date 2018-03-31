{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.Error where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as PP


data LcErr
  = LcPreviouslyConsumed Text Loc
  | LcParamsUnconsumed [Text] Loc
  | LcLamUnconsumed Text Loc
  | LcLetUnconsumed Text Loc
  | LcBranchMismatch [Text] Loc
  | LcUnknownErr
  deriving(Show)

makeClassyPrisms ''LcErr

instance Pretty LcErr where
    pretty = \case
      LcPreviouslyConsumed n l ->
        PP.pretty l
        PP.<+>
        PP.textStrict "Linear variable"
          PP.<+> PP.squotes (PP.textStrict n)
          PP.<+> "was already consumed."
      
      LcParamsUnconsumed ns l ->
        PP.pretty l
        PP.<+>
        PP.textStrict "Variable(s) introduced by function were not consumed:"
          PP.<+> PP.pretty ns

      LcLamUnconsumed n l ->
        PP.pretty l
        PP.<+>
        PP.textStrict "Unconsumed variable"
          PP.<+> PP.squotes (PP.textStrict n)
          PP.<+> PP.textStrict "introduced by lambda expression."

      LcLetUnconsumed n l ->
        PP.pretty l
        PP.<+>
        PP.textStrict "Unconsumed variable"
          PP.<+> PP.squotes (PP.textStrict n)
          PP.<+> PP.textStrict "introduced by let expression."

      LcBranchMismatch ns l ->
        PP.pretty l
        PP.<+>
        PP.textStrict "Mismatch in linear variable consumption between branches in if expression. Variables are"
          PP.<+> PP.vcat (map PP.pretty ns)
          PP.<+> PP.textStrict "introduced by let expression."

      LcUnknownErr ->
        PP.textStrict "Unbound variable encountered:"
