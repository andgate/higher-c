module Language.Hawk.Syntax.Variable where

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Binding as Binding
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source =
  Variable Name.Source Expr.Source (Maybe Type.Source)
  
type Valid =
  Variable Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Variable Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Variable Name.Typed Expr.Typed Type.Typed

type Variable n e t =
  A.Located (Variable' n e t)


data Variable' n e t
  = Variable
    { name  :: Binding.Binding n 
    , tipe  :: t
    , rhs   :: e
    }
  deriving (Show)
  
  
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Variable' n e t) where
  pretty (Variable name tipe rhs) =
    PP.text "Variable:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
        PP.<$>
        PP.text "rhs:" <+> PP.pretty rhs
      )