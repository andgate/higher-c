{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Hawk.Syntax.MetaItem where

import Data.Binary
import Data.Data
import Data.Typeable

import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Syntax.Record as Rec
import qualified Language.Hawk.Syntax.Type as Type


-- MetaItems Structure

type Source = 
  MetaItem Name.Source (Maybe Type.Source)
 
type Valid = 
  MetaItem Name.Valid (Maybe Type.Valid)
  
type Typed =
  MetaItem Name.Typed Type.Typed

   
data MetaItem n t
  = Import (ModuleName.Raw)
  | Function (Fn.FunctionInfo n t)
  | Variable (Var.VariableInfo n t)
  | Record (Rec.Record n)
  | Alias (Alias.Alias n)
  deriving (Eq, Show, Data, Typeable)

  
instance (Binary n, Binary t) => Binary (MetaItem n t) where
  get = do t <- getWord8
           case t of
                0 -> Import <$> get
                1 -> Function <$> get
                2 -> Variable <$> get
                3 -> Record <$> get
                4 -> Alias <$> get
    
  put t = case t of
      Import n ->
          putWord8 0 >> put n
      Function fi ->
          putWord8 1 >> put fi
      Variable oi ->
          putWord8 2 >> put oi
      Record r ->
          putWord8 3 >> put r
      Alias a ->
          putWord8 4 >> put a