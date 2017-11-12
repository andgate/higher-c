{-# LANGUAGE  FlexibleInstances
            , TypeSynonymInstances
            , GeneralizedNewtypeDeriving
  #-}
module Language.Hawk.TypeCheck.Types where

import Data.Default.Class
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

import Language.Hawk.Syntax.Type
import qualified Data.Map as Map

data Constraint

    -- Equality Constraint, to be unified when solving.
  = EqConst Type Type

    -- Explicit Instance Constraint, states Scheme is a generic instance of Type.
    -- Useful if type is known beforehand.
  | ExpInstConst Type Scheme

    -- Implicit Instance Constraint, says that t1 is an instance of t2
    -- when generalized to a Scheme with a given Set of type variables.
    -- This is for handling let-polymorphism - in general, the type of a
    -- declaration of a let-expression must be inferred before it can
    -- be instantiated.
  | ImpInstConst Type (Set Text) Type

  deriving (Show, Eq, Ord)


newtype Subst = Subst (Map Text Type)
  deriving (Eq, Ord, Show, Monoid)


instance Default Subst where
  def = Subst Map.empty
