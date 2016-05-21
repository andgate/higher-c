-- Combinator-based Hawk Parser
module Language.Hawk.Parse where


import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.AST.Module as Module



program :: Package.Name -> String -> IO Module.Source
program _ _ = undefined