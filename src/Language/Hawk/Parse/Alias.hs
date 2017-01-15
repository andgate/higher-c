module Language.Hawk.Parse.Alias where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Alias as Al
  


alias :: HkParser Al.Source
alias = locate $
  Al.Alias <$> (conName <* equals) <*> typ2