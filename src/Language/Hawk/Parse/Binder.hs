module Language.Hawk.Parse.Binder where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Expression
import qualified Language.Hawk.Syntax.Binder as Binder
import qualified Language.Hawk.Report.Region as R


binder :: MonadicParsing m => m Binder.Source
binder = 
  do  p <- position
      
      string "let"
      spaces
      
      mode <- bindingMode
      name <- varName
      spaces
      
      -- t <- type
      spaces
      
      equals
      spaces
      
      --e <- expr
      r <- R.mkRegion p <$> position
      undefined
      --return Binder mode name t e r

bindingMode :: MonadicParsing m => m Binder.BindingMode
bindingMode =
  byRef <|> byVal <?> "Binding Mode"

byVal :: MonadicParsing m => m Binder.BindingMode
byVal = 
  Binder.ByVal <$> mutability

byRef :: MonadicParsing m => m Binder.BindingMode
byRef =
  char '&' *>
  pure Binder.ByRef <*> mutability
                    <?> "Reference Binding"
  

mutability :: MonadicParsing m => m Binder.Mutability
mutability =
  immutable <|> mutable <?> "Mutability Modifier"
  
immutable :: MonadicParsing m => m Binder.Mutability
immutable =
  char '!' *>
  pure Binder.Immutable <?> "Immutable Binding Modifier"

mutable :: MonadicParsing m => m Binder.Mutability
mutable = 
  pure Binder.Mutable <?> "Mutable Binding"