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


expr :: HkParsing m => m Expr.Source
expr = 
      (try exprTyped <?> "Typed Expression")
  <|> (expr0 <?> "Expression")


exprTyped :: HkParsing m => m Expr.Source
exprTyped =
  locate $ Expr.Cast <$> expr0 <*> typesig


expr0 :: HkParsing m => m Expr.Source
expr0 =
      (try fexpr <?> "Function application expresion")
  <|> (aexpr <?> "Basic Expression")


fexpr :: HkParsing m => m Expr.Source
fexpr =
  locate $ do
    (call:args) <- some (try aexpr)
    return (Expr.App call args)


aexpr :: HkParsing m => m Expr.Source
aexpr = 
      (try litExpr <?> "Literal Expression")
  <|> (try varExpr <?> "Variable Binding Expression")
  <|> (try conExpr <?> "Constructor Expression")
  <|> (nestedExpr <?> "Nested Expression")


varExpr :: HkParsing m => m Expr.Source
varExpr =
  locate $ Expr.Var <$> varName


conExpr :: HkParsing m => m Expr.Source
conExpr =
  locate $ Expr.Var <$> conName

litExpr :: HkParsing m => m Expr.Source
litExpr =
  locate $ Expr.Lit <$> literal
  

nestedExpr :: HkParsing m => m Expr.Source
nestedExpr =
  parens $ expr