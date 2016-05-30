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
  locate $ (Binding.Binding <$> bindingMode <*> varName) <?> "Name Binding"



bindingMode :: MonadicParsing m => m Binding.Mode
bindingMode =
  byRef <|> byVal <?> "Binding Mode"

byVal :: MonadicParsing m => m Binding.Mode
byVal = 
  Binding.ByVal <$> mutability

byRef :: MonadicParsing m => m Binding.Mode
byRef =
  char '&' *>
  pure Binding.ByRef <*> mutability
                    <?> "Reference Binding"
  

mutability :: MonadicParsing m => m Binding.Mutability
mutability =
  immutable <|> mutable <?> "Mutability Modifier"
  
immutable :: MonadicParsing m => m Binding.Mutability
immutable =
  char '!' *>
  pure Binding.Immutable <?> "Immutable Binding Modifier"

mutable :: MonadicParsing m => m Binding.Mutability
mutable = 
  pure Binding.Mutable <?> "Mutable Binding"