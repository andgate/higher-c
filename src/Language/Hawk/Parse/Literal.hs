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
      floatLit
  <|> intLit
  <|> charLit
  <|> stringLit
  <|> boolLit
  <?> "Literal"

intLit :: MonadicParsing m => m Lit.Literal
intLit = Lit.IntNum <$> try integer <?> "Integer Literal"


floatLit :: MonadicParsing m => m Lit.Literal
floatLit = Lit.FloatNum <$> try double <?> "Double Literal"


charLit :: MonadicParsing m => m Lit.Literal
charLit = Lit.Chr <$> try charLiteral <?> "Character Literal"


stringLit :: MonadicParsing m => m Lit.Literal
stringLit = Lit.Str <$> try stringLiteral <?> "String Literal"


boolLit :: MonadicParsing m => m Lit.Literal
boolLit = Lit.Boolean <$> (trueBool <|> falseBool) <?> "Boolean Literal"


trueBool :: MonadicParsing m => m Bool
trueBool = try (string "true") *> pure True


falseBool :: MonadicParsing m => m Bool
falseBool = try (string "false") *> pure False