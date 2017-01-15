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


expr :: HkParser Expr.Source
expr = 
      (try exprTyped <?> "Typed Expression")
  <|> (expr0 <?> "Expression")


exprTyped :: HkParser Expr.Source
exprTyped =
  locate $ Expr.Cast <$> expr0 <*> typesig


expr0 :: HkParser Expr.Source
expr0 =
      (try fexpr <?> "Function application expresion")
  <|> (aexpr <?> "Basic Expression")


fexpr :: HkParser Expr.Source
fexpr =
  locate $ do
    (call:args) <- some (try aexpr)
    return (Expr.App call args)


aexpr :: HkParser Expr.Source
aexpr = 
      (try litExpr <?> "Literal Expression")
  <|> (try varExpr <?> "Variable Binding Expression")
  <|> (try conExpr <?> "Constructor Expression")
  <|> (nestedExpr <?> "Nested Expression")


varExpr :: HkParser Expr.Source
varExpr =
  locate $ Expr.Var <$> varName


conExpr :: HkParser Expr.Source
conExpr =
  locate $ Expr.Var <$> conName

litExpr :: HkParser Expr.Source
litExpr =
  locate $ Expr.Lit <$> literal
  

nestedExpr :: HkParser Expr.Source
nestedExpr =
  parens $ expr