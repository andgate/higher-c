module Language.Hawk.Parse.ModuleName where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Compile.Package as Pkg
import qualified Language.Hawk.Syntax.ModuleName as ModuleName


moduleName :: Pkg.Name -> HkParser ModuleName.Name
moduleName pkgName =
  ModuleName.Name pkgName <$> moduleNameRaw

moduleNameRaw :: HkParser ModuleName.Raw
moduleNameRaw =
  modId `sepBy1` period
  