module Language.Hawk.Syntax.Object where

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Binding as Binding
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source =
  Object Name.Source Expr.Source (Maybe Type.Source)
  
type Valid =
  Object Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Object Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Object Name.Typed Expr.Typed Type.Typed

type Object n e t =
  A.Located (Object' n e t)


data Object' n e t
  = Object
    { name  :: Binding.Binding n 
    , tipe  :: t
    , rhs   :: e
    }
  deriving (Show)
  
  
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Object' n e t) where
  pretty (Object name tipe rhs) =
    PP.text "Object:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
        PP.<$>
        PP.text "rhs:" <+> PP.pretty rhs
      )