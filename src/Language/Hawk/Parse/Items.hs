module Language.Hawk.Parse.Items where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Function
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Record
import Language.Hawk.Parse.Variable
import qualified Language.Hawk.Syntax.Items as Items



items :: MonadicParsing m => m Items.Source
items =
   (block item) <?> "items"
  
  
item :: MonadicParsing m => m Items.SourceItem
item = do
  c <- getComment
  idat <- itemData
  return $ Items.basic c idat
  
itemData :: MonadicParsing m => m Items.SourceItemData
itemData =
      (impItem)
  <|> (recordItem)
  <|> (functionItem)
  <|> (varItem)


functionItem :: MonadicParsing m => m Items.SourceItemData
functionItem =
  Items.FunctionItem <$> try function

recordItem :: MonadicParsing m => m Items.SourceItemData
recordItem = 
  Items.RecordItem <$> try record
  
varItem :: MonadicParsing m => m Items.SourceItemData
varItem = 
  Items.VarItem <$> try var
  
impItem :: MonadicParsing m =>  m Items.SourceItemData
impItem = do
  try $ string "->"
  Items.ImportItem <$> lpad moduleNameRaw
  