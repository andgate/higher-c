module Language.Hawk.Syntax.Function where

import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Meta
  = Function Name.Source () (Maybe Type.Source)

type Source
  = Function Name.Source Expr.Source (Maybe Type.Source)

type Valid
  = Function Name.Valid Expr.Valid (Maybe Type.Valid)

type Canonical
  = Function Name.Valid Expr.Canonical (Maybe Type.Canonical)

type Typed
  = Function Name.Typed Expr.Typed Type.Typed


type Function n e t =
  A.Located (Function' n e t)

data Function' n e t
  = Function 
    { fn_name :: n
    , fn_args :: [B.Binding n]
    , fn_type :: t
    , fn_body :: Stmt.Block n e t
    }
  deriving (Eq, Show, Data, Typeable)
  
  
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Function' n e t) where
  pretty (Function name args tipe body) =
    PP.text "Function:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "args:" <+> PP.pretty args
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )