module Language.Hawk.Parse.ModuleName where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Compile.Package as Pkg
import qualified Language.Hawk.Syntax.ModuleName as ModuleName


moduleName :: MonadicParsing m => Pkg.Name -> m ModuleName.Name
moduleName pkgName =
  ModuleName.Name pkgName <$> moduleNameRaw

moduleNameRaw :: MonadicParsing m => m ModuleName.Raw
moduleNameRaw =
  modId `sepBy1` period
  