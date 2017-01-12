module Language.Hawk.Parse.Function where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Type as Ty
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Report.Annotation as A


function :: Parser Fn.Source
function =
  locate $
    Fn.Function <$> (functionInfo <* fndefsym) <*> stmtblock


functionInfo :: Parser Fn.SourceInfo
functionInfo = 
  locate $
    Fn.FunctionInfo <$> varName <*> many (try binding) <*> typesig0