module Language.Hawk.Syntax.Module where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))


import qualified Data.Map as Map
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Expression        as Expression
import qualified Language.Hawk.Syntax.ModuleName        as ModuleName
import qualified Language.Hawk.Syntax.Type              as Type
import qualified Language.Hawk.Syntax.Name              as Name
import qualified Language.Hawk.Docs.AST                 as Docs
import qualified Language.Hawk.Compile.Package          as Package
import qualified Language.Hawk.Compile.Compiler.Version as Compiler
import qualified Text.PrettyPrint.ANSI.Leijen           as PP


type Source =
  Module Name.Source Expression.Source (Maybe Type.Source)
   
data Module n e t
  = Module String [I.Item n e t] 
    deriving(Eq, Show, Data, Typeable)
    
    
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Module n e t) where
  pretty (Module n its) =
    PP.text "Module:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty n
        PP.<$>
        PP.text "items:" <+> PP.pretty its
      )