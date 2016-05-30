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
  some statement


statement :: MonadicParsing m => m Stmt.Source
statement = withFloatingLayout $ stmtCall
  
  
  
stmtCall :: MonadicParsing m => m Stmt.Source
stmtCall = 
  locate $ Stmt.Call <$> fexpr


stmtLet :: MonadicParsing m => m Stmt.Source
stmtLet =
  locate $ Stmt.Let <$> var