module Language.Hawk.AST.Expression where

import qualified Language.Hawk.AST.Literal as Literal
import qualified Language.Hawk.AST.ModuleName as ModuleName
import qualified Language.Hawk.AST.Type as Type
import qualified Language.Hawk.AST.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source
  = Expr Name.Raw R.Region (Maybe Type.Source)

type Valid
  = Expr Name.Raw R.Region (Maybe Type.Valid)

type Canonical
  = Expr Name.Canonical R.Region (Maybe Type.Canonical)

type Typed
  = Expr Name.Canonical R.Region Type.Typed


data Expr n a t
  = Lit (Literal.Literal a) t
  | Var n a t
  | Con n a t
  
  | App (Expr n a t) [Expr n a t] a
  | Let n (Expr n a t) (Expr n a t) a
  -- Probably need to add suport for operators later
  
  | If [(Expr n a t, Expr n a t)] (Expr n a t) a
  
  -- Specific stucture access
  | Access (Expr n a t) (Expr n a t) a
  | RefAccess (Expr n a t) (Expr n a t) a
  
  -- disable case for now, patterns are too complicated
  -- | Case (Expr n a t) [(Pattern.Pattern n a, Expr n b t a)]