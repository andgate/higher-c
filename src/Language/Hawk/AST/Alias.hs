module Language.Hawk.AST.Alias where

import qualified Language.Hawk.AST.Name as Name
import qualified Language.Hawk.AST.Type as Type
import qualified Language.Hawk.Report.Region as R

type Source =
  Alias Name.Source R.Region
  
type Valid =
  Alias Name.Valid R.Region
  
type Canonical =
  Alias Name.Canonical R.Region
  
type Typed =
  Alias Name.Typed R.Region


data Alias n a
  = Alias n (Type.Type n a) a