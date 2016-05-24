module Language.Hawk.Parse.Literal where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Syntax.Literal as Lit


literal :: MonadicParsing m => m Lit.Literal
literal =
      try intLit
  <|> try floatLit
  <|> try charLit
  <|> try stringLit
  <|> boolLit
  <?> "Literal"

intLit :: MonadicParsing m => m Lit.Literal
intLit = Lit.IntNum <$> integer


floatLit :: MonadicParsing m => m Lit.Literal
floatLit = Lit.FloatNum <$> double


charLit :: MonadicParsing m => m Lit.Literal
charLit = Lit.Chr <$> charLiteral


stringLit :: MonadicParsing m => m Lit.Literal
stringLit = Lit.Str <$> stringLiteral


boolLit :: MonadicParsing m => m Lit.Literal
boolLit = Lit.Boolean <$> (try trueBool <|> falseBool) <?> "Boolean literal"


trueBool :: MonadicParsing m => m Bool
trueBool = string "true" *> pure True


falseBool :: MonadicParsing m => m Bool
falseBool = string "false" *> pure False