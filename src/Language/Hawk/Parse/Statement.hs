module Language.Hawk.Parse.Statement where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Type
import Language.Hawk.Parse.Variable
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Report.Region as R


stmtblock :: HkParser Stmt.SourceBlock
stmtblock =
  list statement

statement :: HkParser Stmt.Source
statement =
      (try stmtRet <?> "Return Statement")
  <|> (try stmtCall <?> "Call Statement")
  <|> (try stmtAssign <?> "Assign Statement")
  <|> (stmtVarBind <?> "Variable Bind Statement")

 
mkStmt :: HkParser Stmt.Source' -> HkParser Stmt.Source 
mkStmt = locate . lineFold
  
  
stmtCall :: HkParser Stmt.Source
stmtCall = mkStmt $ 
  Stmt.Call <$> fexpr


stmtVarBind :: HkParser Stmt.Source
stmtVarBind = mkStmt $ 
  Stmt.Let <$> var

  
stmtAssign :: HkParser Stmt.Source
stmtAssign = mkStmt $
  Stmt.Assign <$> varName <*> typesig0 <*> (equals >> expr)
  
  
stmtRet :: HkParser Stmt.Source
stmtRet = mkStmt $
  Stmt.Return <$> (symbol "return" >> expr)