{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Lex.Error where

import Control.Lens

import Language.Hawk.Lex.Token
import Language.Hawk.Syntax (Decl)
import Language.Hawk.Syntax.Location
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>), (<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data LexErr
  = UnproducibleToken Char String Text
  | InvalidCharLit Text
  | IllegalLexerSkip
  deriving(Show)

makeClassyPrisms ''LexErr

instance Pretty LexErr where
    pretty = \case
      UnproducibleToken p cs rest  ->
          P.textStrict "Lexical cannot produce token."
          P.<$>
          P.indent 4
            ( P.textStrict "Previous Char:" <+> P.squotes (P.pretty p) 
              P.<$>
              P.textStrict "Current Chars:" <+> P.textStrict (pack cs)
              P.<$>
              P.textStrict "Rest of file:" <+> P.textStrict rest
            )


      IllegalLexerSkip  ->
          P.textStrict "Lexer performed an illegal skip operation."
