module Language.Hawk.Syntax.Module where

import Data.Binary
import Data.Data
import Data.Typeable
import qualified Data.Map as Map

import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Expression as Expression
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Docs.AST as Docs
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile.Compiler.Version as Compiler
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source =
  Module Name.Source Expression.Source (Maybe Type.Source)
   
data Module n e t
  = Module String [I.Item n e t] 
    deriving(Eq, Show, Data, Typeable)