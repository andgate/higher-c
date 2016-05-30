module Language.Hawk.Syntax.Alias where

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A

type Source =
  Alias Name.Source
  
type Valid =
  Alias Name.Valid
  
type Canonical =
  Alias Name.Canonical
  
type Typed =
  Alias Name.Typed
  

type Alias n =
  A.Located (Alias' n)


data Alias' n
  = Alias n (Type.Type n)
  deriving (Show)