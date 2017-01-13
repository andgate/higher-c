module Language.Hawk.Parse.Binding where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import qualified Language.Hawk.Syntax.Binding as Binding


binding :: HkParsing m => m Binding.Source
binding = locate $ Binding.Binding <$> bindMode <*> varName


bindMode :: HkParsing m => m Binding.Mode
bindMode =
  try byRef <|> byVal


byVal :: HkParsing m => m Binding.Mode
byVal = 
  Binding.ByVal <$> mutability

byRef :: HkParsing m => m Binding.Mode
byRef =
  char '&' *>
  pure Binding.ByRef <*> mutability
  

mutability :: HkParsing m => m Binding.Mutability
mutability =
  try immutable <|> mutable
  
  
immutable :: HkParsing m => m Binding.Mutability
immutable =
  char '!' *>
  pure Binding.Immutable

mutable :: HkParsing m => m Binding.Mutability
mutable = 
  pure Binding.Mutable <?> "Mutable symbol"