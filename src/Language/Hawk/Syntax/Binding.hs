module Language.Hawk.Syntax.Binding where

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A


type Source =
  Binding Name.Source
  
type Valid =
  Binding Name.Valid
  
type Canonical =
  Binding Name.Canonical

type Typed =
  Binding Name.Typed
  

type Binding n =
  A.Located (Binding' n)

data Binding' n
  = Binding
    { mode  :: Mode
    , label :: n
    }
  deriving (Show)
  
data Mode
  = ByRef Mutability
  | ByVal Mutability
  deriving (Show)
  
data Mutability
  = Mutable
  | Immutable
  deriving (Show)