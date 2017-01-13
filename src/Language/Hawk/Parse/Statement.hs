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


stmtblock :: HkParsing m => m Stmt.SourceBlock
stmtblock =
  list statement

statement :: HkParsing m => m Stmt.Source
statement =
      (try stmtRet <?> "Return Statement")
  <|> (try stmtCall <?> "Call Statement")
  <|> (try stmtAssign <?> "Assign Statement")
  <|> (stmtVarBind <?> "Variable Bind Statement")

 
mkStmt :: HkParsing m => m Stmt.Source' -> m Stmt.Source 
mkStmt = locate . lineFold
  
  
stmtCall :: HkParsing m => m Stmt.Source
stmtCall = mkStmt $ 
  Stmt.Call <$> fexpr


stmtVarBind :: HkParsing m => m Stmt.Source
stmtVarBind = mkStmt $ 
  Stmt.Let <$> var

  
stmtAssign :: HkParsing m => m Stmt.Source
stmtAssign = mkStmt $
  Stmt.Assign <$> varName <*> typesig0 <*> (equals >> expr)
  
  
stmtRet :: HkParsing m => m Stmt.Source
stmtRet = mkStmt $
  Stmt.Return <$> (symbol "return" >> expr)