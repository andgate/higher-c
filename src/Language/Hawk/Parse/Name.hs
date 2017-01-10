module Language.Hawk.Parse.Name where


import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Syntax.Name as Name


varName :: Parser Name.Source
varName = varId <?> "Variable Name"

conName :: Parser Name.Source
conName = conId <?> "Constructor Name"