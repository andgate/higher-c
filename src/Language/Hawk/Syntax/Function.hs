module Language.Hawk.Syntax.Function where

import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name

import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source
  = Function Name.Source Expr.Source (Maybe Type.Source)

type Valid
  = Function Name.Valid Expr.Valid (Maybe Type.Valid)

type Canonical
  = Function Name.Valid Expr.Canonical (Maybe Type.Canonical)

type Typed
  = Function Name.Typed Expr.Typed Type.Typed


type Function n e t =
  A.Located (Function' n e t)

data Function' n e t
  = Function 
    { fn_name :: n
    , fn_type :: t
    , fn_args :: [B.Binding n]
    , fn_body :: Stmt.Block n e t
    }
  deriving (Show)