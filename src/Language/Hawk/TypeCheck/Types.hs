{-# LANGUAGE  FlexibleInstances
            , TypeSynonymInstances
            , GeneralizedNewtypeDeriving
  #-}
module Language.Hawk.TypeCheck.Types where

import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import Language.Hawk.Syntax.Type


data Constraint
  = EqConst Type Type
  | ExpInstConst Type Scheme
  | ImpInstConst Type (Set Text) Type
  deriving (Show, Eq, Ord)


newtype Subst = Subst (Map Text Type)
  deriving (Eq, Ord, Show, Monoid)
