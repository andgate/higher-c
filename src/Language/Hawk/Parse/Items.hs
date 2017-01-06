module Language.Hawk.Parse.Items where

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




items :: MonadicParsing m => m Items.Source
items =
  blockLayout item
  
  
item :: MonadicParsing m => m Items.SourceItem
item =
      (try impItem <?> "Import")
  <|> (try recordItem <?> "Record")
  <|> (varItem <?> "Function or Object Definition")


itemize :: MonadicParsing m => m Items.SourceItem' -> m Items.SourceItem
itemize = commented


recordItem :: MonadicParsing m => m Items.SourceItem
recordItem = itemize $
  Items.recItem <$> record
  
varItem :: MonadicParsing m => m Items.SourceItem
varItem = itemize $ do
  bs <- many (try binding)
  t <- typesig0
  (try (declFnItem bs t) <?> "Function")
    <|> ((declObjItem bs t) <?> "Object")
 
 
declFnItem :: MonadicParsing m => [B.Source] -> Maybe Ty.Source -> m Items.SourceItem'
declFnItem bs t =
  Items.fnItem <$> declareFunction bs t
  
declObjItem :: MonadicParsing m => [B.Source] -> Maybe Ty.Source -> m Items.SourceItem'
declObjItem bs t =
  Items.objItem <$> declareObj bs t
  
impItem :: MonadicParsing m =>  m Items.SourceItem
impItem = itemize $
  stringTok "->" >> (Items.impItem <$> moduleNameRaw)
  