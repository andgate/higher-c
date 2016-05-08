module Language.Hawk.Syntax.Pattern where

import qualified Data.Set as Set

import qualified Language.Hawk.Syntax.Literal as L
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Pattern ann var =
  A.Annotated ann (Pattern' ann var)
  
  
data Pattern' ann var
  = Data var [Pattern ann var]
  | Record [String]
  | Alias BindingMode String (Pattern ann var)
  | Var BindingMode String
  | Anything
  | Literal L.Literal

  
data BindingMode
  = ByRef Mutability
  | ByVal Mutability
  
data Mutability
  = Mutable
  | Immutable


type Raw =
  Pattern R.Region Var.Raw
  

type Raw' =
  Pattern' R.Region Var.Raw
  
  
type Canonical =
  Pattern R.Region Var.Canonical
  

isVar :: String -> Pattern ann var -> Bool
isVar name (A.A _ pattern) =
  case pattern of
    Var _ pName ->
      name == pName
    
    _ ->
      False
      
      
list :: R.Position -> [Raw] -> Raw
list end patterns =
  case patterns of
    [] ->
      A.at end end (Data (Var.Raw "[]") [])
      
    pattern@(A.A (R.Region start _) _) : rest ->
      A.at start end (Data (Var.Raw ".:") [pattern, list end rest])
      

consMany :: R.Position -> [Raw] -> Raw
consMany end patterns =
  let cons hd@(A.A (R.Region start _) _) tl =
        A.at start end (Data (Var.Raw ".:") [hd, tl])
  in
    foldr1 cons patterns
    
    
boundVars :: Pattern ann var -> [A.Annotated ann String]
boundVars (A.A ann pattern) =
  case pattern of
    Var _ x ->
      [A.A ann x]
    
    Alias _ name realPattern ->
      A.A ann name : boundVars realPattern
      
    Data _ patterns ->
      concatMap boundVars patterns
      
    Record fields ->
      map (A.A ann) fields
      
    Anything ->
      []
      
    Literal _ ->
      []
      

member :: String -> Pattern ann var -> Bool
member name =
  any (name==) . map A.drop . boundVars 
  

boundVarSet :: Pattern ann var -> Set.Set String
boundVarSet =
  Set.fromList . map A.drop . boundVars
 
 
boundVarList :: Pattern ann var -> [String]
boundVarList =
  Set.toList . boundVarSet