module Language.Hawk.Syntax.MetaItems where

import Data.Binary

import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Record as Record
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


-- MetaItems Structure

type Source = 
  Items Name.Source Expr.Source (Maybe Type.Source)
  
type SourceItem = 
  Item Name.Source Expr.Source (Maybe Type.Source)
  
type SourceItem' = 
  Item' Name.Source Expr.Source (Maybe Type.Source)
 
type Valid = 
  Items Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Items Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Items Name.Typed Expr.Typed Type.Typed
  
type Items n e t
  = [Item n e t]

  
type MetaItem n e t = A.Located (MetaItem' n e t) 
   
data MetaItem' n e t
  = Import (ModuleName.Raw)
  | Function n [n] t
  | Object n t
  | Record [(n, t)]
  | Alias n t
  deriving (Show)
  