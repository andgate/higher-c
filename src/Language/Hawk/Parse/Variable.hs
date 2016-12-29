module Language.Hawk.Parse.Variable where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Variable as Var


var :: MonadicParsing m => m Var.Source
var = locate $
  Var.Variable <$> binding <*> typesig0 <*> (vardefsym >> expr)