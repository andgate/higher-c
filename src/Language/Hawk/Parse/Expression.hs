module Language.Hawk.Parse.Expression where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Literal
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Report.Region as R


expr :: MonadicParsing m => m Expr.Source
expr = try exprTyped <|> aexpr


exprTyped :: MonadicParsing m => m Expr.Source
exprTyped = locate $ Expr.Cast <$> aexpr <*> typesig


aexpr :: MonadicParsing m => m Expr.Source
aexpr = litExpr


litExpr :: MonadicParsing m => m Expr.Source
litExpr = locate $ Expr.Lit <$> literal