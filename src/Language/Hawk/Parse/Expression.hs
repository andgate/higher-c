module Language.Hawk.Parse.Expression where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Literal
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Report.Region as R


expr :: Parser Expr.Source
expr = 
      (try exprTyped <?> "Typed Expression")
  <|> (expr0 <?> "Expression")


exprTyped :: Parser Expr.Source
exprTyped =
  locate $ Expr.Cast <$> expr0 <*> typesig


expr0 :: Parser Expr.Source
expr0 =
      (try fexpr <?> "Function application expresion")
  <|> (aexpr <?> "Basic Expression")


fexpr :: Parser Expr.Source
fexpr =
  locate $ do
    (call:args) <- some (try aexpr)
    return (Expr.App call args)


aexpr :: Parser Expr.Source
aexpr = 
      (try litExpr <?> "Literal Expression")
  <|> (try varExpr <?> "Variable Binding Expression")
  <|> (try conExpr <?> "Constructor Expression")
  <|> (nestedExpr <?> "Nested Expression")


varExpr :: Parser Expr.Source
varExpr =
  locate $ Expr.Var <$> varName


conExpr :: Parser Expr.Source
conExpr =
  locate $ Expr.Var <$> conName

litExpr :: Parser Expr.Source
litExpr =
  locate $ Expr.Lit <$> literal
  

nestedExpr :: Parser Expr.Source
nestedExpr =
  parens $ expr