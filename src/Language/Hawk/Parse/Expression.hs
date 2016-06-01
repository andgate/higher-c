module Language.Hawk.Parse.Expression where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Literal
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Report.Region as R


expr :: MonadicParsing m => m Expr.Source
expr = 
      try exprTyped
  <|> expr0


exprTyped :: MonadicParsing m => m Expr.Source
exprTyped =
  locate $ Expr.Cast <$> expr0 <*> lpad typesig


expr0 :: MonadicParsing m => m Expr.Source
expr0 =
      try fexpr
  <|> aexpr


fexpr :: MonadicParsing m => m Expr.Source
fexpr =
  locate $ do
    (call:args) <- spaceSep1 aexpr
    return (Expr.App call args)


aexpr :: MonadicParsing m => m Expr.Source
aexpr = 
      litExpr
  <|> varExpr
  <|> nestedExpr


varExpr :: MonadicParsing m => m Expr.Source
varExpr =
  locate $ Expr.Var <$> varName


litExpr :: MonadicParsing m => m Expr.Source
litExpr =
  locate $ Expr.Lit <$> literal
  

nestedExpr :: MonadicParsing m => m Expr.Source
nestedExpr =
  parens $ expr