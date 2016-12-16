module Language.Hawk.Parse.Module where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Items
import qualified Language.Hawk.Syntax.Items as Items
import qualified Language.Hawk.Syntax.Module as Module


moduleUnits :: MonadicParsing m => m [Module.SourceInfo]
moduleUnits =
  some (moduleInfo)

moduleInfo :: MonadicParsing m => m Module.SourceInfo
moduleInfo =
  do
      string "mod"
      
      n <- lpad moduleNameRaw
      its <- items
      let imps = Items.findImports its
      
      pure $ Module.SourceInfo n imps its