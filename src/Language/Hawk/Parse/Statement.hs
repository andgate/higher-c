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


stmtblock :: MonadicParsing m => m Stmt.SourceBlock
stmtblock =
  blockLayout statement

statement :: MonadicParsing m => m Stmt.Source
statement =
      (try stmtRet <?> "Return Statement")
  <|> (try stmtCall <?> "Call Statement")
  <|> (try stmtAssign <?> "Assign Statement")
  <|> (stmtVarBind <?> "Variable Bind Statement")

  
stmtCall :: MonadicParsing m => m Stmt.Source
stmtCall = 
  locate $ Stmt.Call <$> fexpr <?> "Function Call Statement"


stmtVarBind :: MonadicParsing m => m Stmt.Source
stmtVarBind =
  locate $ Stmt.Let <$> var <?> "Variable Binding"
  
stmtAssign :: MonadicParsing m => m Stmt.Source
stmtAssign = locate $
  Stmt.Assign <$> varName <*> typesig0 <* equals <*> (ws >> expr)
  
  
stmtRet :: MonadicParsing m => m Stmt.Source
stmtRet =
  locate $ Stmt.Return <$> (string "return" >> try ws >> expr)