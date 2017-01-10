module Language.Hawk.Parse.Items where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Function
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Record
import Language.Hawk.Parse.Object
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Items as Items
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Ty



items :: Parser Items.Source
items =
  list item
  
item :: Parser Items.SourceItem
item =
      (try impItem <?> "Import")
  <|> (try recordItem <?> "Record")
  <|> (varItem <?> "Function or Object Definition")


recordItem :: Parser Items.SourceItem
recordItem = locate $
  Items.recItem <$> record
  
varItem :: Parser Items.SourceItem
varItem = locate $ do
  bs <- many (try binding)
  t <- typesig0
  (try (declFnItem bs t) <?> "Function")
    <|> ((declObjItem bs t) <?> "Object")
 
 
declFnItem :: [B.Source] -> Maybe Ty.Source -> Parser Items.SourceItem'
declFnItem bs t =
  Items.fnItem <$> declareFunction bs t
  
declObjItem :: [B.Source] -> Maybe Ty.Source -> Parser Items.SourceItem'
declObjItem bs t =
  Items.objItem <$> declareObj bs t
  
impItem :: Parser Items.SourceItem
impItem = locate $
  rightArrow >> (Items.impItem <$> moduleNameRaw)
  