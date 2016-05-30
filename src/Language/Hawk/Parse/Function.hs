module Language.Hawk.Parse.Function where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Function as Fn


function :: MonadicParsing m => m Fn.Source
function =
  locate $ do
      string "fn"
      ws
      name <- varName
      ws
      tipe <- typesig
      ws
      string "|"
      ws
      args <- spaceSep binding
      ws
      string "="
      ws
      body <- block
      return $ Fn.Function name tipe args body