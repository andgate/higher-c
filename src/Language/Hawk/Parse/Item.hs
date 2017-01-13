module Language.Hawk.Parse.Item where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Function
import Language.Hawk.Parse.Record
import Language.Hawk.Parse.Variable
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Ty



items :: HkParsing m => m  [I.Source]
items = 
  nonIndented $ list item
  
item :: HkParsing m => m  I.Source
item =
      (try impItem <?> "Import")
  <|> (try recordItem <?> "Record")
  <|> (try fnItem <?> "Function")
  <|> (varItem <?> "Object")


  
impItem :: HkParsing m => m  I.Source
impItem = locate $
  rightArrow >> (I.impItem <$> moduleNameRaw)
  
recordItem :: HkParsing m => m  I.Source
recordItem = locate $
  I.recItem <$> record

fnItem :: HkParsing m => m  I.Source   
fnItem = locate $
  I.fnItem <$> function
  
varItem :: HkParsing m => m I.Source   
varItem = locate $
  I.varItem <$> var
  