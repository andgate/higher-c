module Language.Hawk.Parse.Literal where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Syntax.Literal as Lit

literal :: HkParsing m => m Lit.Literal
literal =
      floatLit 
  <|> intLit
  <|> charLit
  <|> stringLit
  <|> boolLit
  <?> "Literal"


intLit ::  HkParsing m => m Lit.Literal
intLit = Lit.IntNum <$> signedInteger <?> "Integer Literal"


floatLit :: HkParsing m => m Lit.Literal
floatLit = Lit.FloatNum <$> signedFloat <?> "Double Literal"


charLit :: HkParsing m => m Lit.Literal
charLit = Lit.Chr <$> qchar <?> "Character Literal"


stringLit :: HkParsing m => m Lit.Literal
stringLit = Lit.Str <$> qstring <?> "String Literal"


boolLit :: HkParsing m => m Lit.Literal
boolLit = Lit.Boolean <$> (try trueBool <|> falseBool) <?> "Boolean Literal"


trueBool :: HkParsing m => m Bool
trueBool = string "true" *> pure True


falseBool :: HkParsing m => m Bool
falseBool = string "false" *> pure False