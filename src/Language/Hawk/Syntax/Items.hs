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
 
type Valid = 
  Items Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Items Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Items Name.Typed Expr.Typed Type.Typed
 
type Items n e t
  = [Item n e t]
  

   
   
data Item n e t
  = Item Comment Visibility (ItemData n e t)
  deriving (Show)

data Comment
  = Comment String
  deriving (Show)
  
data Visibility
  = Public
  | Private
  deriving (Show)


data ItemData n e t
  = ImportItem (ModuleName.Raw)
  | FunctionItem (Fn.Function n e t)
  | VarItem (Var.Variable n e t)
  | RecordItem (Record.Record n)
  | AliasItem (Alias.Alias n)
  deriving (Show)


itemizeFunction :: Fn.Function n e t -> Item n e t
itemizeFunction fn =
  basic (FunctionItem fn)


findImports :: Source -> Source
findImports = id
  
emptyComment :: Comment
emptyComment = Comment ""

noComment :: Visibility -> ItemData n e t -> Item n e t
noComment = Item emptyComment

basic :: ItemData n e t -> Item n e t
basic = Item emptyComment Public
