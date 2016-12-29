module Language.Hawk.Syntax.Items where

import Data.Binary

import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Record as Record
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R
  
-- Items Structure

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
  

type Item n e t = A.Commented (Item' n e t) 
   
data Item' n e t
  = ImportItem Visibility (ModuleName.Raw)
  | FunctionItem Visibility (Fn.Function n e t)
  | VarItem Visibility (Var.Variable n e t)
  | RecordItem Visibility (Record.Record n)
  | AliasItem Visibility (Alias.Alias n)
  deriving (Show)
  
  
data Visibility
  = Public
  | Private
  deriving (Show)

impItem :: ModuleName.Raw -> Item' n e t
impItem = ImportItem Public

fnItem :: Fn.Function n e t -> Item' n e t
fnItem = FunctionItem Public

varItem :: Var.Variable n e t -> Item' n e t
varItem = VarItem Public

recItem :: Record.Record n -> Item' n e t
recItem = RecordItem Public

aliasItem :: Alias.Alias n -> Item' n e t
aliasItem = AliasItem Public


findImports :: Source -> Source
findImports = filter isImport

isImport :: Item n e t -> Bool
isImport (A.A _ (ImportItem _ _)) = True
isImport _ = False
  
