module Language.Hawk.Syntax.Type where

import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source'
  = Type' Name.Source

type Source
  = Type Name.Source

type Valid
  = Type Name.Valid

type Canonical
  = Maybe (Type Name.Canonical)

type Typed
  = Type Name.Canonical


type Type n = A.Located (Type' n)

data Type' n
  = App (Type n) [Type n]
  | Con n
  deriving (Show)
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
  
{-
fn :: [Source] -> R.Region -> Source
fn types region =
  let
    name = 
      Name.Name ("_Arr" ++ show (length types)) region
  in
    App (Con name region) types
-}

arrow :: R.Region -> [Source] -> Source
arrow =
  variadic "_Arr"

tuple :: R.Region -> [Source] -> Source
tuple =
  variadic "_Tuple"

variadic :: String -> R.Region -> [Source] -> Source
variadic name region types =
  let
    con = 
      A.A region $ Con name
  in
    A.A region $ App con types
      

instance (Binary n) => Binary (Type' n) where
  put tipe =
    case tipe of
      App t1 t2 ->
        putWord8 0 >> put t1 >> put t2
        
      Con name ->
        putWord8 1 >> put name
        
      --Var name a ->
      --  putWord8 2 >> put name >> put a
        
  get =
    do  n <- getWord8
        case n of
          0 -> App <$> get <*> get
          1 -> Con <$> get
          --2 -> Var <$> get
          _ -> error "Error reading a valid type from serialized string"
          