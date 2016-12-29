module Language.Hawk.Parse.Binding where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Name
import qualified Language.Hawk.Syntax.Binding as Binding


binding :: MonadicParsing m => m Binding.Source
binding =
  locate $ (Binding.Binding <$> evaluation <*> varName) <?> "Name Binding"



evaluation :: MonadicParsing m => m Binding.Mode
evaluation =
  byRef <|> byVal

byVal :: MonadicParsing m => m Binding.Mode
byVal = 
  Binding.ByVal <$> mutability

byRef :: MonadicParsing m => m Binding.Mode
byRef =
  try (charTok '&') *>
  pure Binding.ByRef <*> mutability
  

mutability :: MonadicParsing m => m Binding.Mutability
mutability =
  immutable <|> mutable
  
immutable :: MonadicParsing m => m Binding.Mutability
immutable =
  try (charTok '!') *>
  pure Binding.Immutable

mutable :: MonadicParsing m => m Binding.Mutability
mutable = 
  pure Binding.Mutable <?> "Mutable symbol"