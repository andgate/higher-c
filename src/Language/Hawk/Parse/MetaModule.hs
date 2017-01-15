module Language.Hawk.Parse.MetaModule where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.MetaItem
import qualified Language.Hawk.Syntax.MetaItem as MI
import qualified Language.Hawk.Syntax.MetaModule as MM


metamodl :: HkParser MM.Source
metamodl = 
  MM.MetaModule <$> metaItems <* eof