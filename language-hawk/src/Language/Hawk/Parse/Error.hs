{-# LANGUAGE LambdaCase #-}
module Language.Hawk.Parse.Error where


import Data.Text.Prettyprint.Doc
import Language.Hawk.Lex.Token


data ParseError
    = UnexpectedToken [Token] [String]
    | AmbiguousGrammar
    deriving(Show)

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]
    
        AmbiguousGrammar ->
            pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."