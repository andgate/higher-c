module Language.Hawk.AST.Items where

import Data.Binary

import qualified Language.Hawk.AST.Alias as Alias
import qualified Language.Hawk.AST.Binder as Binder
import qualified Language.Hawk.AST.Expression as Expr
import qualified Language.Hawk.AST.Function as Function
import qualified Language.Hawk.AST.Name as Name
import qualified Language.Hawk.AST.Record as Record
import qualified Language.Hawk.AST.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R
  
-- Items Structure

type Source = 
  Items Name.Source Expr.Source R.Region Type.Source
 
type Valid = 
  Items Name.Valid Expr.Valid R.Region Type.Valid
  
type Canonical =
  Items Name.Canonical Expr.Canonical R.Region Type.Canonical
  
type Typed =
  Items Name.Typed Expr.Typed R.Region Type.Canonical
  
data Items n e a t
  = Items
    { _fns      :: [Function n e a t]
    , _binds    :: [Binder n e a t]
    , _recs     :: [Record n a]
    , _aliases  :: [Alias n a]
    }
    
data Item i a
  = Item (Comment a) (Visibility a) i

data Comment a
  = Comment String a
  
data Visibility a
  = Public a
  | Private a


-- Specific Items  
type Function n e a t
  = Item (Function.Function n e a t) a
  
type Binder n e a t
  = Item (Binder.Binder n e a t) a
  
type Record n a
  = Item (Record.Record n a) a
  
type Alias n a
  = Item (Alias.Alias n a) a
  

  
addFunction :: Function n e a t -> Items n e a t -> Items n e a t
addFunction fn items =
  items { _fns = fn : _fns items }
  
addBinder :: Binder n e a t -> Items n e a t -> Items n e a t
addBinder bind items =
  items { _binds = bind : _binds items }
  
addRecord :: Record n a -> Items n e a t -> Items n e a t
addRecord rec items =
  items { _recs = rec : _recs items }
  
addAlias :: Alias n a -> Items n e a t -> Items n e a t
addAlias alias items =
  items { _aliases = alias : _aliases items }