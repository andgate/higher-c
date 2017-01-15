module Language.Hawk.Parse.Module where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Item
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Module as M


modl :: HkParser  M.Source
modl = 
  M.Module "" <$> items <* eof