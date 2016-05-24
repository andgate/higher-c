module Language.Hawk.Syntax.Expression where

import qualified Language.Hawk.Syntax.Literal as Literal
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source
  = Expr Name.Raw Type.Source

type Valid
  = Expr Name.Raw Type.Valid

type Canonical
  = Expr Name.Canonical Type.Canonical

type Typed
  = Expr Name.Canonical Type.Typed

type Expr n t =
  A.Located (Expr' n t)

data Expr' n t
  = Lit Literal.Literal
  | Var n
  | Con n
  
  | App (Expr n t) [Expr n t]
  | Let n (Expr n t) (Expr n t)
  -- Probably need to add suport for operators later
  
  | If [(Expr n t, Expr n t)] (Expr n t)
  
  -- Specific stucture access
  | Access (Expr n t) (Expr n t)
  | RefAccess (Expr n t) (Expr n t)
  
  | Cast (Expr n t) t
  
  -- disable case for now, patterns are too complicated
  -- | Case (Expr n t) [(Pattern.Pattern n, Expr n t)]
  
  deriving (Show)