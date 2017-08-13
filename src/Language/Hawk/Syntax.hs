{-# LANGUAGE TypeFamilies
           , GADTs
           , DeriveGeneric
           , DataKinds
           , ConstraintKinds
           , KindSignatures
           , EmptyCase
           , StandaloneDeriving
           , TypeOperators
           , PatternSynonyms
           , FlexibleInstances
           , FlexibleContexts
           , OverloadedStrings
           , UndecidableInstances
           , TemplateHaskell
           , LambdaCase
           , DeriveDataTypeable
           , BangPatterns
  #-}
module Language.Hawk.Syntax
  ( module Language.Hawk.Syntax
  , module Language.Hawk.Syntax.DataDecl
  , module Language.Hawk.Syntax.Decl
  , module Language.Hawk.Syntax.Literal
  , module Language.Hawk.Syntax.Kind
  , module Language.Hawk.Syntax.Location
  , module Language.Hawk.Syntax.Name
  , module Language.Hawk.Syntax.Prim
  , module Language.Hawk.Syntax.Term
  , module Language.Hawk.Syntax.TypeLiteral
  ) where

import Control.Lens
import Data.Binary
import Data.Data hiding (Fixity, DataType)
import Data.Data.Lens (uniplate)
import Data.Default.Class
import Data.Either
import Data.Map.Lazy (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Tree
import GHC.Types (Constraint)
import GHC.Generics (Generic)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax.DataDecl
import Language.Hawk.Syntax.Decl
import Language.Hawk.Syntax.Expression
import Language.Hawk.Syntax.Kind
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Term
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.TypeLiteral
import Text.PrettyPrint.Leijen.Text ((<+>))


import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Map.Lazy                as Map
import qualified Data.Set                     as Set



{-
-- -----------------------------------------------------------------------------
-- | New Type

data NewType
  = NewType
    { _newTyName     :: Name
    , _newTyNewBody  :: Type
    } deriving (Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias
    = TypeAlias
      { _tyAliasName   :: Name
      , _tyAliasBody   :: Type
      } deriving (Show, Eq, Data, Typeable, Generic)

-}
