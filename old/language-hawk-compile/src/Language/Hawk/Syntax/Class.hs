{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , TupleSections
            , DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
  #-}
module Language.Hawk.Syntax.Class where

import Bound
import Bound.Scope
import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Telescope


-------------------------------------------------------------------------------
-- Class definitions
newtype Class typ v = Class { _classMethods :: [Method (TeleScope typ v)] }
  deriving (Foldable, Functor, Show, Traversable)


data Method typ = Method
  { _methodName  :: Text
  , _methodLoc   :: Loc
  , _methodType  :: typ
  } deriving (Foldable, Functor, Show, Traversable)

