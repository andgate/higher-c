module Language.Hawk.Syntax.Expression.Canonical where

import qualified Language.Hawk.Syntax.Expression.General as General
import qualified Language.Hawk.Syntax.Pattern as Pattern
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Expr =
  General.Expr R.Region Def Var.Canonical Type.Canonical
  
type Expr' =
  General.Expr' R.Region Def Var.Canonical Type.Canonical
  
  
data Def =
  Def Facts Pattern.Canonical Expr (Maybe (A.Located Type.Canonical))

data Facts
  = Facts
    { dependencies :: [Var.TopLevel]
    }

dummyFacts :: Facts
dummyFacts =
  Facts (error "This should be set by Canonicalize.Sort")
  

data SortedDefs
  = NoMain [Def]
  | YesMain [Def] Def [Def]
  
  
toSortedDefs :: Expr -> SortedDefs
toSortedDefs (A.A _ expr) =
  case expr of
    General.Let defs body ->
      foldr defCons (toSortedDefs body) defs
      
    _ ->
      NoMain []
      

defCons :: Def -> SortedDefs -> SortedDefs
defCons def@(Def _ (A.A _ pattern) _ _) sortedDefs =
  case (pattern, sortedDefs) of
    (Pattern.Var _ "main", NoMain defs) ->
      YesMain [] def defs
      
    (_, NoMain defs) ->
      NoMain (def : defs)
      
    (_, YesMain defs main rest) ->
      YesMain (def:defs) main rest