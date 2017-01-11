module Language.Hawk.Syntax.Item where

import Data.Binary
import Data.Data
import Data.Typeable

import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Record as Record
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Object as Obj
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R
  
-- Items Structure
  
type Source = 
  Item Name.Source Expr.Source (Maybe Type.Source)
  
type Source' = 
  Item' Name.Source Expr.Source (Maybe Type.Source)
 
type Valid = 
  Item Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Item Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Item Name.Typed Expr.Typed Type.Typed

type Item n e t = A.Located (Item' n e t) 
   
data Item' n e t
  = ImportItem Visibility (ModuleName.Raw)
  | FunctionItem Visibility (Fn.Function n e t)
  | ObjItem Visibility (Obj.Object n e t)
  | RecordItem Visibility (Record.Record n)
  | AliasItem Visibility (Alias.Alias n)
  deriving (Eq, Show, Data, Typeable)
  
  
data Visibility
  = Public
  | Private
  deriving (Eq, Show, Data, Typeable)

impItem :: ModuleName.Raw -> Item' n e t
impItem = ImportItem Public

fnItem :: Fn.Function n e t -> Item' n e t
fnItem = FunctionItem Public

objItem :: Obj.Object n e t -> Item' n e t
objItem = ObjItem Public

recItem :: Record.Record n -> Item' n e t
recItem = RecordItem Public

aliasItem :: Alias.Alias n -> Item' n e t
aliasItem = AliasItem Public


findImports :: [Source] -> [Source]
findImports = filter isImport

isImport :: Item n e t -> Bool
isImport (A.A _ (ImportItem _ _)) = True
isImport _ = False
  
