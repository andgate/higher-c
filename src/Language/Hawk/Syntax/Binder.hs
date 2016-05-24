module Language.Hawk.Syntax.Binder where

import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R

type Source =
  Binder Name.Source Expr.Source Type.Source

type Binder n e t =
  A.Located (Binder' n e t)

data Binder' n e t
  = Binder
    { mode  :: BindingMode
    , bame  :: n
    , tipe  :: t
    , rhs   :: e
    }
  deriving (Show)
  
data BindingMode
  = ByRef Mutability
  | ByVal Mutability
  deriving (Show)
  
data Mutability
  = Mutable
  | Immutable
  deriving (Show)