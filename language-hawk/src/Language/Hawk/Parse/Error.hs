{-# LANGUAGE LambdaCase #-}
module Language.Hawk.Parse.Error where


import Data.Text.Prettyprint.Doc
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Source (TopLevelDef)


data ParseError
    = UnexpectedToken [Token] [String]
    | AmbiguousGrammar [TopLevelDef]
    deriving(Show)

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]
    
        AmbiguousGrammar parses ->
            vcat [ pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."
                 , vcat (pretty <$> parses)
                 ]