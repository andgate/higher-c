module Language.Hawk.Syntax.Record where

import Data.Data
import Data.Typeable

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A

type Source =
  Record Name.Source
  
type Valid =
  Record Name.Valid
  
type Canonical =
  Record Name.Canonical
  
type Typed =
  Record Name.Typed
  

type Record n =
  A.Located (Record' n)

data Record' n
  = Record
    { name :: n
    , fields :: [RecordField n]
    }
  deriving (Eq, Show, Data, Typeable)


type RecordField n
  = A.Located (RecordField' n)

data RecordField' n
  = RecordField n (Type.Type n)
  deriving (Eq, Show, Data, Typeable)