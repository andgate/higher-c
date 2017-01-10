module Language.Hawk.Parse.Module where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Items
import qualified Language.Hawk.Syntax.Items as Items
import qualified Language.Hawk.Syntax.Module as Module


moduleInfo :: Parser Module.SourceInfo
moduleInfo = do
      its <- items <* eof
      let imps = Items.findImports its
      pure $ Module.SourceInfo [] imps its