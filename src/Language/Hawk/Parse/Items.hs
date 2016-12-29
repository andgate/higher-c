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
  blockLayout item
  
  
item :: MonadicParsing m => m Items.SourceItem
item =
      (try impItem <?> "Import Item")
  <|> (try recordItem <?> "Record Item")
  <|> (try varItem <?> "Variable Item")
  <|> (functionItem <?> "Function Item")


itemize :: MonadicParsing m => m Items.SourceItem' -> m Items.SourceItem
itemize = commented . lineLayout


functionItem :: MonadicParsing m => m Items.SourceItem
functionItem = itemize $
  Items.fnItem <$> function

recordItem :: MonadicParsing m => m Items.SourceItem
recordItem = itemize $
  Items.recItem <$> record
  
varItem :: MonadicParsing m => m Items.SourceItem
varItem = itemize $
  Items.varItem <$> var
  
impItem :: MonadicParsing m =>  m Items.SourceItem
impItem = itemize $
  stringTok "->" >> (Items.impItem <$> moduleNameRaw)
  