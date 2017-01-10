-- Combinator-based Hawk Parser
module Language.Hawk.Parse where

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Module
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Syntax.Module as Module


program :: Package.Name -> String -> IO [Module.Source]
program _ _ = undefined


