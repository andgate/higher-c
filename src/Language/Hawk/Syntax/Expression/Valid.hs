module Language.Hawk.Syntax.Expression.Valid where

import qualified Language.Hawk.Syntax.Expression.General as General
import qualified Language.Hawk.Syntax.Pattern as Pattern
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Region as R


type Expr =
  General.Expr R.Region Def Var.Raw Type.Raw
  
type Expr' =
  General.Expr' R.Region Def Var.Raw Type.Raw
  
  
data Def =
  Def Pattern.Raw Expr (Maybe Type.Raw)
  
  
getPattern :: Def -> Pattern.Raw
getPattern (Def pattern _ _) =
  pattern