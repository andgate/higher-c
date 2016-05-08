module Language.Hawk.Syntax.Expression.Source where

import qualified Language.Hawk.Syntax.Expression.General as General
import qualified Language.Hawk.Syntax.Pattern as Pattern
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Expr =
  General.Expr R.Region Def Var.Raw Type.Raw
  
type Expr' =
  General.Expr' R.Region Def Var.Raw Type.Raw
  
  
type Def =
  A.Located Def'
  
data Def'
  = Definition Pattern.Raw Expr
  | Annotation String Type.Raw