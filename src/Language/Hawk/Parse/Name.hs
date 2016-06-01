module Language.Hawk.Parse.Name where


import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Syntax.Name as Name


varName :: MonadicParsing m => m Name.Source
varName = varId <?> "Variable Name"

conName :: MonadicParsing m => m Name.Source
conName = conId <?> "Constructor Name"