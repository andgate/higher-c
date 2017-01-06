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
import Language.Hawk.Parse.Object
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
  <|> (stmtObjBind <?> "Object Bind Statement")

  
stmtCall :: MonadicParsing m => m Stmt.Source
stmtCall = locate . lineLayout $ 
  Stmt.Call <$> fexpr <?> "Function Call Statement"


stmtObjBind :: MonadicParsing m => m Stmt.Source
stmtObjBind = locate . lineLayout $
  Stmt.Let <$> obj <?> "Variable Binding"

  
stmtAssign :: MonadicParsing m => m Stmt.Source
stmtAssign = locate . lineLayout $
  Stmt.Assign <$> varName <*> typesig0 <*> (equals >> expr)
  
  
stmtRet :: MonadicParsing m => m Stmt.Source
stmtRet = locate . lineLayout $
  Stmt.Return <$> (stringTok "return" >> expr)