module Language.Hawk.Syntax.Expression where

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Literal as Literal
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source
  = Expr Name.Source (Maybe Type.Source)

type Valid
  = Expr Name.Valid (Maybe Type.Valid)

type Canonical
  = Expr Name.Canonical (Maybe Type.Canonical)

type Typed
  = Expr Name.Typed Type.Typed

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
  
  | Cast (Expr n t) (Type.Type n)
  
  -- disable case for now, patterns are too complicated
  -- | Case (Expr n t) [(Pattern.Pattern n, Expr n t)]
  
  deriving (Show)
  
  
  
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (Expr' n t) where
  pretty (Lit lit) =
    PP.text "Literal Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty lit )