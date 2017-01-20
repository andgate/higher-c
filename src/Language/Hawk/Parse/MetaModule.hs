module Language.Hawk.Parse.MetaModule where

import Control.Applicative
import Text.Earley

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.MetaItem
import qualified Language.Hawk.Syntax.MetaItem as MI
import qualified Language.Hawk.Syntax.MetaModule as MM
import qualified Language.Hawk.Parse.Lexer as L


metamodl :: HkParser MM.Source
metamodl = 
  MM.MetaModule <$> metaItems <* eof