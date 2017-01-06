module Language.Hawk.Parse.Function where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Type as Ty
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Report.Annotation as A


declareFunction :: MonadicParsing m => [B.Source] -> Maybe Ty.Source -> m Fn.Source
declareFunction ((A.A _ n):bs) t = locate $
  (fndefsym >> (Fn.Function (B.label n) bs t <$> stmtblock))


function :: MonadicParsing m => m Fn.Source
function =
  locate $
    Fn.Function <$> varName <*> many binding <*> typesig0 <*> (fndefsym >> stmtblock)


--fnExpr :: MonadicParsing m => m Stmt.SourceBlock
--fnExpr =
--  string ":=" *> withLayout (Stmt.mkRetBlk <$> expr)