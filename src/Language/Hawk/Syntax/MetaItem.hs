{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Hawk.Syntax.MetaItem where

import Data.Binary
import Data.Data
import Data.Typeable

import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Record as Record
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


-- MetaItems Structure

type Source = 
  MetaItem Name.Source (Maybe Type.Source)
  
type Source' = 
  MetaItem' Name.Source (Maybe Type.Source)
 
type Valid = 
  MetaItem Name.Valid (Maybe Type.Valid)
  
type Typed =
  MetaItem Name.Typed Type.Typed

  
type MetaItem n t = A.Located (MetaItem' n t) 
   
data MetaItem' n t
  = Import (ModuleName.Raw)
  | Function n [n] t
  | Object n t
  | Record n [(n, t)]
  | Alias n t
  deriving (Eq, Show, Data, Typeable)

  
instance (Binary n, Binary t) => Binary (MetaItem' n t) where
  get = do t <- get :: Get Word8
           case t of
                0 -> Import <$> get
                1 -> Function <$> get <*> get <*> get
                2 -> Object <$> get <*> get
                3 -> Record <$> get <*> get
                4 -> Alias <$> get <*> get
    
  put t = case t of
      Import n ->
          put (0 :: Word8) >> put n
      Function n args t ->
          put (1 :: Word8) >> put n
      Object n t ->
          put (2 :: Word8) >> put n >> put t
      Record n fs ->
          put (3 :: Word8) >> put n >> put fs
      Alias n t ->
          put (2 :: Word8) >> put n >> put t