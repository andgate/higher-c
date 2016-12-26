module Language.Hawk.Parse.Record where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type

import qualified Language.Hawk.Syntax.Record as Rec
import qualified Language.Hawk.Syntax.Name as Name


record :: MonadicParsing m => m Rec.Source
record = 
  locate $ do
    n <- ws >> conName
    ws >> condefsym
    xs <- ws >> record_fields
    
    return $ Rec.Record n xs
    
    
record_fields :: MonadicParsing m => m [Rec.RecordField Name.Source]
record_fields =
  block record_field


record_field :: MonadicParsing m => m (Rec.RecordField Name.Source)
record_field = locate $ do
  n <- ws >> varName
  t <- ws >> typesig
  return $ Rec.RecordField n t