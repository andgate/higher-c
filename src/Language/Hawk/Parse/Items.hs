module Language.Hawk.Parse.Items where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Function
import qualified Language.Hawk.Syntax.Items as Items



items :: MonadicParsing m => m Items.Source
items = undefined


functionItem :: MonadicParsing m => m Items.SourceFunction
functionItem = 
  Items.noComment <$> visibility <*> function

visibility :: MonadicParsing m => m Items.Visibility
visibility =
  string "public" *> pure Items.Public
