module Language.Hawk.Parse.Binding where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import qualified Language.Hawk.Syntax.Binding as Binding


binding :: HkParser Binding.Source
binding = locate $ Binding.Binding <$> bindMode <*> varName


bindMode :: HkParser Binding.Mode
bindMode =
  try byRef <|> byVal


byVal :: HkParser Binding.Mode
byVal = 
  Binding.ByVal <$> mutability

byRef :: HkParser Binding.Mode
byRef =
  char '&' *>
  pure Binding.ByRef <*> mutability
  

mutability :: HkParser Binding.Mutability
mutability =
  try immutable <|> mutable
  
  
immutable :: HkParser Binding.Mutability
immutable =
  char '!' *>
  pure Binding.Immutable

mutable :: HkParser Binding.Mutability
mutable = 
  pure Binding.Mutable <?> "Mutable symbol"