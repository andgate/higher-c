module Language.Hawk.Syntax.Function where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Type


type Source
  = Function Name.Source Expr.Source (Maybe Type.Source)
  
type SourceInfo
  = FunctionInfo Name.Source (Maybe Type.Source)

type Valid
  = Function Name.Valid Expr.Valid (Maybe Type.Valid)

type Canonical
  = Function Name.Valid Expr.Canonical (Maybe Type.Canonical)

type Typed
  = Function Name.Typed Expr.Typed Type.Typed


data Function n e t
  = Function 
    { fn_info :: FunctionInfo n t
    , fn_body :: Stmt.Block n e t
    }
  deriving (Eq, Show, Data, Typeable)
 
  
data FunctionInfo n t
  = FunctionInfo 
    { fn_name :: n
    , fn_args :: [B.Binding n]
    , fn_type :: t
    }
  deriving (Eq, Show, Data, Typeable)
  
  
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Function n e t) where
  pretty (Function info body) =
    PP.text "Function:"
    PP.<$>
    PP.indent 2
      ( PP.text "info:" <+> PP.pretty info
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )
      
      
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (FunctionInfo n t) where
  pretty (FunctionInfo name args tipe) =
    PP.text "FunctionInfo:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "args:" <+> PP.pretty args
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
      )
  
  
instance (Binary n, Binary t) => Binary (FunctionInfo n t) where
  get =
      FunctionInfo <$> get <*> get <*> get
      
  put (FunctionInfo n as t) =
      put n >> put as >> put t