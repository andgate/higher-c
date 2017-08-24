{-# LANGUAGE  DeriveGeneric 
            , DeriveFunctor
            , DeriveFoldable
            , DeriveTraversable
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , TypeSynonymInstances
            , FlexibleInstances
            , MultiParamTypeClasses
  #-}
module Language.Hawk.Syntax.Term where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Binary (Binary(..))
import Data.Data
import Data.Data.Lens (uniplate)
import Data.Default.Class
import Data.Monoid
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes

import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.TypeLiteral

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.TH (makeClosedAlpha)

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Term

type TName = Name Term

type Type = Term

data Kind
  = Star
  | Pop
  | Box
  deriving (Show, Eq, Ord, Generic)

data Term
  = Var TName
  | App Term Term
  | Lam (Bind (TName, Embed Annot) Term)
  | Pi  (Bind (TName, Embed Term) Term)

  | Let (Bind (TName, Embed Term) Term)
  | If Term Term Term Annot

  | Con   Text
  | Lit   Lit
  | TyLit TLit
  | Prim  PrimInstr

  | Copy Term
  | Free TName Term

  | Ann   Term Term
  | Paren Term
  | TLoc  Location Term
  | Hole

  | Kind Kind
  deriving (Show, Generic)


newtype Annot = Annot (Maybe Term)
  deriving (Show, Generic)

instance Binary Kind

instance Binary Term where
  put t = undefined
  get = undefined

instance Binary Annot

-- -----------------------------------------------------------------------------
-- | Instances

instance Default Term where
  def = Hole

-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

star :: Term
star = Kind Star

pop :: Term
pop = Kind Pop

box :: Term
box = Kind Box


-- | Default name for '_' occurring in patterns
wildcardName :: TName
wildcardName = string2Name "_"

-- -----------------------------------------------------------------------------
-- | Alpha Equivalence, Free Variables, and Substitution

-- Location
$(makeClosedAlpha ''Location)
$(makeClosedAlpha ''Text)
$(makeClosedAlpha ''Lit)
$(makeClosedAlpha ''TLit)
$(makeClosedAlpha ''PrimInstr)
$(makeClosedAlpha ''Kind)

instance Subst b Location   where subst _ _ = id ; substs _ = id
instance Subst b Text       where subst _ _ = id ; substs _ = id
instance Subst b Lit        where subst _ _ = id ; substs _ = id
instance Subst b TLit       where subst _ _ = id ; substs _ = id
instance Subst b PrimInstr  where subst _ _ = id ; substs _ = id
instance Subst b Kind       where subst _ _ = id ; substs _ = id


-- Term
instance Alpha Term where
  
instance Alpha Annot where
    -- override default behavior so that type annotations are ignored
    -- when comparing for alpha-equivalence
    aeq' _ _ _ = True


instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

instance Subst Term Annot

-- -----------------------------------------------------------------------------
-- | Pretty Printing

instance PP.Pretty TName where
  pretty = PP.textStrict . pack . name2String

instance PP.Pretty Term where
  pretty t = undefined