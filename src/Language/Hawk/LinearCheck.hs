{-# LANGUAGE  OverloadedStrings
            , FlexibleInstances
            , FlexibleContexts
            , GADTs
            , LambdaCase
            , MultiParamTypeClasses
            , FunctionalDependencies
            , RankNTypes
            , TemplateHaskell
            , GeneralizedNewtypeDeriving
            , TypeSynonymInstances
            , UndecidableInstances
            , StandaloneDeriving
  #-}
module Language.Hawk.LinearCheck where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

import Data.Bag
import Data.Default.Class
import Data.List (lookup, union, concatMap, nub, find, delete, intersect)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Syntax
import Language.Hawk.LinearCheck.Environment (Env)
import Language.Hawk.LinearCheck.Error
import Language.Hawk.LinearCheck.Message
import Language.Hawk.LinearCheck.State


import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.LinearCheck.Environment as Env


-----------------------------------------------------------------------
-- Classes
-----------------------------------------------------------------------
