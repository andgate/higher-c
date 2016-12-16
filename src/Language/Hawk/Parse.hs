-- Combinator-based Hawk Parser
module Language.Hawk.Parse where

import Text.Parser.Char hiding (alphaNum, anyChar, space)
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Parser


import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Module
import Language.Hawk.Parse.Layout
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Syntax.Module as Module


program :: Package.Name -> String -> IO [Module.Source]
program _ _ = undefined


