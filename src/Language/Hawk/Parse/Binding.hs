module Language.Hawk.Parse.Binding where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import qualified Language.Hawk.Syntax.Binding as Binding


binding :: Parser Binding.Source
binding = locate $ Binding.Binding <$> bindMode <*> varName


bindMode :: Parser Binding.Mode
bindMode =
  try byRef <|> byVal


byVal :: Parser Binding.Mode
byVal = 
  Binding.ByVal <$> mutability

byRef :: Parser Binding.Mode
byRef =
  char '&' *>
  pure Binding.ByRef <*> mutability
  

mutability :: Parser Binding.Mutability
mutability =
  try immutable <|> mutable
  
  
immutable :: Parser Binding.Mutability
immutable =
  char '!' *>
  pure Binding.Immutable

mutable :: Parser Binding.Mutability
mutable = 
  pure Binding.Mutable <?> "Mutable symbol"