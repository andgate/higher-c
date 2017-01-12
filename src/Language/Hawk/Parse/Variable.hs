module Language.Hawk.Parse.Variable where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Type as Ty
  


var :: Parser Var.Source
var = locate $
  Var.Variable <$> (varInfo <* vardefsym) <*> expr
  
  
varInfo  :: Parser Var.SourceInfo
varInfo = locate $
  Var.VariableInfo <$> binding <*> typesig0