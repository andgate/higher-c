{-# LANGUAGE LambdaCase #-}
module Language.Hawk.Parse.Error where


import Data.Text.Prettyprint.Doc
import Language.Hawk.Lex.Token


data ParseError
    = UnexpectedToken [Token]
    | AmbiguousGrammar
    deriving(Show)

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken ts ->
            pretty "Unexpected tokens:" <+> dquotes (pretty ts)
    
        AmbiguousGrammar ->
            pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."