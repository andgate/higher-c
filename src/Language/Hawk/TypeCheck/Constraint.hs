{-# LANGUAGE DeriveGeneric, LambdaCase  #-}
module Language.Hawk.TypeCheck.Constraint where

import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Term

import qualified Data.Set as Set

-----------------------------------------------------------------------
-- Type Checker Constraints
-----------------------------------------------------------------------

data Constraint

    -- Equality Constraint, to be unified when solving.
  = EqConst Term Term

    -- Explicit Instance Constraint, states second type is a generic
    -- instance of the first.
    -- Useful if type is known beforehand.
  | ExpInstConst Term Term

    -- Implicit Instance Constraint, says that t1 is an instance of t2
    -- when generalized to a Scheme with a given Set of type variables.
    -- This is for handling let-polymorphism - in general, the type of a
    -- declaration of a let-expression must be inferred before it can
    -- be instantiated.
  | ImpInstConst Term (Set Text) Term

  deriving (Show, Eq, Ord, Generic)


-----------------------------------------------------------------------
-- Active Type Variables
-----------------------------------------------------------------------

class ActiveTypeVars a where
  atv :: a -> Set Text

instance ActiveTypeVars Constraint where
  atv = \case
    EqConst t1 t2          -> ftv t1 `Set.union` ftv t2
    ImpInstConst t1 ms t2  -> ftv t1 `Set.union` (ftv ms `Set.intersection` ftv t2) 
    ExpInstConst t s       -> ftv t `Set.union` ftv s 


instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr (Set.union . atv) Set.empty

