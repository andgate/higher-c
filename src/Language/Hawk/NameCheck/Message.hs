{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.NameCheck.Message where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Data.Text as T
import qualified Text.PrettyPrint.Leijen.Text as P

data NcMsg
  = NcStarted [Text]
  | NcFinished
  | NcHit Text Loc
  deriving(Show)

makeClassyPrisms ''NcMsg

instance Pretty NcMsg where
    pretty = \case
      NcStarted ns ->
        P.textStrict "Name checker has"
          P.<+> P.pretty ns
      
      NcFinished ->
        P.textStrict "Name check completed."

      NcHit n l ->
        P.pretty l
          P.<> P.textStrict ": Found symbol"
          P.<+> P.dquotes (P.textStrict n)
