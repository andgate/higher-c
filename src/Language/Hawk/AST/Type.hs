module Language.Hawk.AST.Type where

import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map

import qualified Language.Hawk.AST.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R



type Source
  = Type Name.Source R.Region

type Valid
  = Type Name.Valid R.Region

type Canonical
  = Maybe (Type Name.Canonical R.Region)

type Typed
  = Type Name.Canonical R.Region


data Type n a
  = App (Type n a) [Type n a] a
  | Con n a
-- Type variables will require at least monomorphization,
-- and won't be possible for a while.
--  | Var TVar a

type TVar = String

  
  

-- These are built-in type constructors. The definition of type does not
-- directly contain these, for the sake of generality.

-- Arr (Type n) (Type n)
-- "_Arrow"

-- Primitive types
-- A list of different types, such as "_I32" and "_F32"

-- Ptr (Type n)
-- "_Pointer"

-- Array (Type n)
-- "_Array"

-- Vector (Type n)
-- "_Vector"

-- Tuple [Type n]
-- "_Tuple"
  

tuple :: R.Region -> [Source] -> Source
tuple region types =
  let
    name = 
      Name.Name region ("_Tuple" ++ show (length types))
  in
    App (Con name region) types region
    
      

instance (Binary n, Binary a) => Binary (Type n a) where
  put tipe =
    case tipe of
      App t1 t2 a ->
        putWord8 0 >> put t1 >> put t2 >> put a
        
      Con name a ->
        putWord8 1 >> put name >> put a
        
      --Var name a ->
      --  putWord8 2 >> put name >> put a
        
  get =
    do  n <- getWord8
        case n of
          0 -> App <$> get <*> get <*> get
          1 -> Con <$> get <*> get
          --2 -> Var <$> get <*> get
          _ -> error "Error reading a valid type from serialized string"
          