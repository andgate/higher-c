module Language.Hawk.Parse.Statement where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Type
import Language.Hawk.Parse.Object
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Report.Region as R


stmtblock :: Parser Stmt.SourceBlock
stmtblock =
  list statement

statement :: Parser Stmt.Source
statement =
      (try stmtRet <?> "Return Statement")
  <|> (try stmtCall <?> "Call Statement")
  <|> (try stmtAssign <?> "Assign Statement")
  <|> (stmtObjBind <?> "Object Bind Statement")

 
mkStmt :: Parser Stmt.Source' -> Parser Stmt.Source 
mkStmt = locate
  
  
stmtCall :: Parser Stmt.Source
stmtCall = mkStmt $ 
  Stmt.Call <$> fexpr


stmtObjBind :: Parser Stmt.Source
stmtObjBind = mkStmt $ 
  Stmt.Let <$> obj

  
stmtAssign :: Parser Stmt.Source
stmtAssign = mkStmt $
  Stmt.Assign <$> varName <*> typesig0 <*> (equals >> expr)
  
  
stmtRet :: Parser Stmt.Source
stmtRet = mkStmt $
  Stmt.Return <$> (symbol "return" >> expr)