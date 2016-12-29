module Language.Hawk.Parse.Function where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Statement as Stmt


function :: MonadicParsing m => m Fn.Source
function =
  locate $
    Fn.Function <$> (varName <* ws) <*> (spaceSep binding <* ws) <*> (typesig0 <* (ws >> fndefsym)) <*> stmtblock
  


--fnExpr :: MonadicParsing m => m Stmt.SourceBlock
--fnExpr =
--  string ":=" *> withLayout (Stmt.mkRetBlk <$> expr)