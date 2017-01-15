module Language.Hawk.Parse.Literal where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Syntax.Literal as Lit

literal :: HkParser Lit.Literal
literal =
      floatLit 
  <|> intLit
  <|> charLit
  <|> stringLit
  <|> boolLit
  <?> "Literal"


intLit :: HkParser Lit.Literal
intLit = Lit.IntNum <$> signedInteger <?> "Integer Literal"


floatLit :: HkParser Lit.Literal
floatLit = Lit.FloatNum <$> signedFloat <?> "Double Literal"


charLit :: HkParser Lit.Literal
charLit = Lit.Chr <$> qchar <?> "Character Literal"


stringLit :: HkParser Lit.Literal
stringLit = Lit.Str <$> qstring <?> "String Literal"


boolLit :: HkParser Lit.Literal
boolLit = Lit.Boolean <$> (try trueBool <|> falseBool) <?> "Boolean Literal"


trueBool :: HkParser Bool
trueBool = string "true" *> pure True


falseBool :: HkParser Bool
falseBool = string "false" *> pure False