module Language.Hawk.Syntax.Binder where

import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R

type Source =
  Binder Name.Source Expr.Source Type.Source

type Binder n e t =
  A.Located (Binder' n e t)

data Binder' n e t
  = Binder BindingMode n t e
  
data BindingMode
  = ByRef Mutability
  | ByVal Mutability
  
data Mutability
  = Mutable
  | Immutable