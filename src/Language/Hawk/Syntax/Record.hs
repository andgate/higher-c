module Language.Hawk.Syntax.Record where

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Region as R

type Source =
  Record Name.Source R.Region
  
type Valid =
  Record Name.Valid R.Region
  
type Canonical =
  Record Name.Canonical R.Region
  
type Typed =
  Record Name.Typed R.Region


data Record n a
  = Record n [RecordField n a] a
  
  
data RecordField n a
  = RecordField n (Type.Type n) a