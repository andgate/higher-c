module Language.Hawk.Parse.Function where

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
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Statement as Stmt


function :: MonadicParsing m => m Fn.Source
function =
  freshLine *> function'

function' :: MonadicParsing m => m Fn.Source
function' =
  locate $ do
      string "fn"
      name <- lpad varName
      tipe <- lpad typesig0  
      
      try $ case tipe of
          Just _  -> lpad (string "|")
          Nothing -> return []
      args <- lpad (spaceSep binding)
      
      (Fn.Function name tipe args) <$> lpad fnBody


      
fnBody :: MonadicParsing m => m Stmt.SourceBlock
fnBody =
      fnBlock

fnBlock :: MonadicParsing m => m Stmt.SourceBlock
fnBlock =
  string ":" *> block <?> "Statement block"

fnExpr :: MonadicParsing m => m Stmt.SourceBlock
fnExpr =
  string "=" *> lpad (Stmt.mkRetBlk <$> expr)