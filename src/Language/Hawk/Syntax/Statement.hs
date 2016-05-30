module Language.Hawk.Syntax.Statement where

import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R

type Source
  = Statement Name.Raw Expr.Source Type.Source
  
type SourceBlock
  = Block Name.Raw Expr.Source Type.Source


type Valid
  = Statement Name.Raw Expr.Valid Type.Valid
  
type ValidBlock
  = Block Name.Raw Expr.Source Type.Source


type Canonical
  = Statement Name.Canonical Expr.Canonical Type.Canonical

type CanonicalBlock
  = Block Name.Raw Expr.Source Type.Source


type Typed
  = Statement Name.Canonical Expr.Typed Type.Typed
  
type TypedBlock
  = Block Name.Raw Expr.Source Type.Source
  


type Block n e t = [Statement n e t]


type Statement n e t =
  A.Located (Statement' n e t)


data Statement' n e t
  = Do (Block n e t)
  | Call e
  | Let (Var.Variable n e t)
  | Assign n t e
  | Break
  | Return e
  | If [(e, Block n e t)] (Block n e t)
  | While e (Block n e t)
  deriving (Show)