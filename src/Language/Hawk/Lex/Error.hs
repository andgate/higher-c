{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Lex.Error where

import Control.Lens

import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data LxErr
  = UnproducibleToken String Loc
  | InvalidCharLit Text
  | IllegalLexerSkip
  deriving(Show)

makeClassyPrisms ''LxErr

instance Pretty LxErr where
    pretty = \case
      UnproducibleToken cs l  ->
          P.textStrict "Lexer has failed on"
            P.<+> P.dquotes (P.textStrict $ pack cs)
            P.<+> P.textStrict "at"
            P.<+> P.pretty l

      IllegalLexerSkip  ->
          P.textStrict "Lexer performed an illegal skip operation."
