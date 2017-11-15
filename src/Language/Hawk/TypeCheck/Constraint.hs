{-# LANGUAGE DeriveGeneric  #-}
module Language.Hawk.TypeCheck.Constraint where

import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Type

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

  deriving (Show, Eq, Ord, Generic)

