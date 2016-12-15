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
  freshLine >> statement'


statement' :: MonadicParsing m => m Stmt.Source
statement' = 
      try stmtAssign
  <|> try stmtVarBind
  <|> try stmtRet
  <|> stmtCall
  <?> "Statement"

  
stmtCall :: MonadicParsing m => m Stmt.Source
stmtCall = 
  locate $ Stmt.Call <$> fexpr <?> "Function Call Statement"


stmtVarBind :: MonadicParsing m => m Stmt.Source
stmtVarBind =
  locate $ Stmt.Let <$> var <?> "Variable Binding"
  
stmtAssign :: MonadicParsing m => m Stmt.Source
stmtAssign =
  locate $ do
    n <- varName
    t <- lpad typesig0
    
    pad equals
    
    e <- expr
    
    (return $ Stmt.Assign n t e)
  
  
stmtRet :: MonadicParsing m => m Stmt.Source
stmtRet =
  locate $ Stmt.Return <$> (string "return" *> ws *> expr)