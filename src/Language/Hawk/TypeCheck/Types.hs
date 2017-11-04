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
  = EqConst Type Type
  | ExpInstConst Type Scheme
  | ImpInstConst Type (Set Text) Type
  deriving (Show, Eq, Ord)


newtype Subst = Subst (Map Text Type)
  deriving (Eq, Ord, Show, Monoid)


instance Default Subst where
  def = Subst Map.empty
