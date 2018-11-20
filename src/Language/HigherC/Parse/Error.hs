{-# LANGUAGE LambdaCase #-}
module Language.HigherC.Parse.Error where

import Data.Text.Prettyprint.Doc
import Language.HigherC.Lex.Error
import Language.HigherC.Lex.Token
import Language.HigherC.Syntax.Concrete (Stmt)


data ParseError
    = UnexpectedToken [Token] [String]
    | AmbiguousGrammar [Stmt]
    | PLexErr LexError

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]
    
        AmbiguousGrammar srcs ->
            vcat [ pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."
                 , vcat (pretty <$> srcs)
                 ]

        PLexErr err ->
            pretty err
