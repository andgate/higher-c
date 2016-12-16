module Language.Hawk.Parse.Items where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import qualified Language.Hawk.Parse.Function as Fn
import qualified Language.Hawk.Syntax.Items as Items



items :: MonadicParsing m => m Items.Source
items =
   concat <$> (withLayout . some) item
  
  
item :: MonadicParsing m => m Items.Source
item =
  functionItem

functionItem :: MonadicParsing m => m Items.Source
functionItem = do
  fn <- (Items.itemizeFunction <$> Fn.function)
  pure [fn]
