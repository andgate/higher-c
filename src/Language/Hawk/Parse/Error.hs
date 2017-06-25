{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Parse.Error where

import Control.Lens

import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax (ItemPs)
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P

data ParseErr
  = UnexpectedToken Token
  | AmbiguousGrammar [[ItemPs]]
  deriving(Show)

makeClassyPrisms ''ParseErr

instance Pretty ParseErr where
    pretty err =
      case err of
        UnexpectedToken t ->
            P.textStrict "Unexpected token"
              <+> P.squotes (P.textStrict $ t^.tokText)
              <+> P.textStrict "at" <+> P.pretty (t^.tokLoc)
    
        AmbiguousGrammar ps ->
            P.textStrict "Ambigiuos grammar detected:" P.<$> P.textStrict (pack $ show ps)