module Language.Hawk.Parse.Literal where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Syntax.Literal as Lit


literal :: Parser Lit.Literal
literal =
      floatLit
  <|> intLit
  <|> charLit
  <|> stringLit
  <|> boolLit
  <?> "Literal"


intLit :: Parser Lit.Literal
intLit = Lit.IntNum <$> signedInteger <?> "Integer Literal"


floatLit :: Parser Lit.Literal
floatLit = Lit.FloatNum <$> signedFloat <?> "Double Literal"


charLit :: Parser Lit.Literal
charLit = Lit.Chr <$> qchar <?> "Character Literal"


stringLit :: Parser Lit.Literal
stringLit = Lit.Str <$> qstring <?> "String Literal"


boolLit :: Parser Lit.Literal
boolLit = Lit.Boolean <$> (try trueBool <|> falseBool) <?> "Boolean Literal"


trueBool :: Parser Bool
trueBool = string "true" *> pure True


falseBool :: Parser Bool
falseBool = string "false" *> pure False