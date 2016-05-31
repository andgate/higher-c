module Language.Hawk.Parse.Statement where

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
import Language.Hawk.Parse.Type
import Language.Hawk.Parse.Variable
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Report.Region as R


block :: MonadicParsing m => m Stmt.SourceBlock
block =
  withLayout $ some statement


statement :: MonadicParsing m => m Stmt.Source
statement =
  freshLine *> statement'


statement' :: MonadicParsing m => m Stmt.Source
statement' = 
      try stmtCall
  <|> try stmtLet
  <|> stmtRet
  <?> "Statement"

  
stmtCall :: MonadicParsing m => m Stmt.Source
stmtCall = 
  locate $ Stmt.Call <$> fexpr <?> "Function Call Statement"


stmtLet :: MonadicParsing m => m Stmt.Source
stmtLet =
  locate $ Stmt.Let <$> var <?> "Let Statement"
  
  
stmtRet :: MonadicParsing m => m Stmt.Source
stmtRet =
  locate $ Stmt.Return <$> (string "return" *> ws *> expr)