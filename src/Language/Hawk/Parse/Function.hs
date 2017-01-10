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


declareFunction :: [B.Source] -> Maybe Ty.Source -> Parser Fn.Source
declareFunction [] _ = error "cannot declare function with no name"
declareFunction ((A.A _ n):bs) t = locate $
  (fndefsym >> (Fn.Function (B.label n) bs t <$> stmtblock))


function :: Parser Fn.Source
function =
  locate $
    Fn.Function <$> varName <*> many binding <*> typesig0 <*> (fndefsym >> stmtblock)


--fnExpr :: Parser Stmt.SourceBlock
--fnExpr =
--  string ":=" *> withLayout (Stmt.mkRetBlk <$> expr)