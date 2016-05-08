module Language.Hawk.Syntax.Expression.General where

import qualified Language.Hawk.Syntax.Literal as Literal
import qualified Language.Hawk.Syntax.Module.Name as ModuleName
import qualified Language.Hawk.Syntax.Pattern as Pattern
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A


type Expr annotation definition variable tipe =
  A.Annotated annotation (Expr' annotation definition variable tipe)
  
  
data Expr' ann def var typ
  = Literal Literal.Literal
  | Var var
  | Binop var (Expr ann def var typ) (Expr ann def var typ)
  | Lambda (Pattern.Pattern ann var) (Expr ann def var typ)
  | App (Expr ann def var typ) (Expr ann def var typ)
  | If [(Expr ann def var typ, Expr ann def var typ)] (Expr ann def var typ)
  | Let [def] (Expr ann def var typ)
  | Case (Expr ann def var typ) [(Pattern.Pattern ann var, Expr ann def var typ)]
  | Data String [Expr ann def var typ]
  | Access (Expr ann def var typ) String
  | Update (Expr ann def var typ) [(String, Expr ann def var typ)]
  | Record [(String, Expr ann def var typ)]


rawVar :: String -> Expr' ann def Var.Raw typ
rawVar x =
  Var (Var.Raw x)
  

localVar :: String -> Expr' ann def Var.Canonical typ
localVar x =
  Var (Var.Canonical Var.Local x)
  

tuple :: [Expr ann def var typ] -> Expr' ann def var typ
tuple expressions =
  Data ("_Tuple" ++ show (length expressions)) expressions
  

collectApps :: Expr ann def var typ -> [Expr ann def var typ]
collectApps annExpr@(A.A _ expr) =
  case expr of
    App a b ->
      collectApps a ++ [b]
      
    _ ->
      [annExpr]
      
collectLambdas :: Expr ann def var typ -> ([Pattern.Pattern ann var], Expr ann def var typ)
collectLambdas lexpr@(A.A _ expr) =
  case expr of
    Lambda pattern body ->
      let
        (ps, body') = collectLambdas body 
      in 
        (pattern : ps, body')
    
    _ ->
      ([], lexpr)