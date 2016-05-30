module Language.Hawk.Syntax.Variable where


import qualified Language.Hawk.Syntax.Binding as Binding
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R

type Source =
  Variable Name.Source Expr.Source Type.Source
  
type Valid =
  Variable Name.Valid Expr.Valid Type.Valid
  
type Canonical =
  Variable Name.Canonical Expr.Canonical Type.Canonical
  
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