module Language.Hawk.Compile where

import qualified Data.Map as Map
import Data.Function ((&))

import qualified Language.Hawk.AST.Items as Items
import qualified Language.Hawk.AST.Expression as Expr
import qualified Language.Hawk.AST.Module as Module
import qualified Language.Hawk.AST.ModuleName as ModuleName
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Parse as Parse
import qualified Language.Hawk.Report.Error as Error
import qualified Language.Hawk.Report.Result as Result
import qualified Language.Hawk.Report.Warning as Warning


type Result =
  Result.Result Warning.Warning Error.Error Module.Source
  
  
compile
  :: Package.Name
  -> String
  -> IO Result
compile packageName sourcePath =
  do
      srcModule <- Parse.program packageName sourcePath
            
      return $ Right ([], srcModule)