module Language.Hawk.Syntax.Items where

import Data.Binary

import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Function as Function
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Record as Record
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R
  
-- Items Structure

type Source = 
  Items Name.Source Expr.Source Type.Source
 
type Valid = 
  Items Name.Valid Expr.Valid Type.Valid
  
type Canonical =
  Items Name.Canonical Expr.Canonical Type.Canonical
  
type Typed =
  Items Name.Typed Expr.Typed Type.Canonical
  
data Items n e t
  = Items
    { _fns      :: [Function n e t]
    , _vars     :: [Variable n e t]
    , _recs     :: [Record n]
    , _aliases  :: [Alias n]
    }
  deriving (Show)
    
data Item i
  = Item Comment Visibility i
  deriving (Show)

data Comment
  = Comment String
  deriving (Show)
  
data Visibility
  = Public
  | Private
  deriving (Show)


-- Specific Items  
type Function n e t
  = Item (Function.Function n e t)
  
type Variable n e t
  = Item (Var.Variable n e t)
  
type Record n
  = Item (Record.Record n)
  
type Alias n
  = Item (Alias.Alias n)
  

  
addFunction :: Function n e t -> Items n e t -> Items n e t
addFunction fn items =
  items { _fns = fn : _fns items }
  
addBinder :: Variable n e t -> Items n e t -> Items n e t
addBinder var items =
  items { _vars = var : _vars items }
  
addRecord :: Record n -> Items n e t -> Items n e t
addRecord rec items =
  items { _recs = rec : _recs items }
  
addAlias :: Alias n -> Items n e t -> Items n e t
addAlias alias items =
  items { _aliases = alias : _aliases items }