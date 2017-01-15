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



items :: HkParser  [I.Source]
items = 
  nonIndented $ list item
  
item :: HkParser  I.Source
item =
      (impItem <?> "Import")
  <|> (recordItem <?> "Record")
  <|> (fnItem <?> "Function")
  <|> (varItem <?> "Object")


  
impItem :: HkParser I.Source
impItem = locate $
  rightArrow >> (I.impItem <$> moduleNameRaw)
  
recordItem :: HkParser I.Source
recordItem = locate $
  I.recItem <$> record

fnItem :: HkParser I.Source   
fnItem = locate $
  I.fnItem <$> function
  
varItem :: HkParser I.Source   
varItem = locate $
  I.varItem <$> var
  