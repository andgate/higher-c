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
  #-}
module Language.Hawk.Syntax.Term where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Binary (Binary)
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

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Term

type TName = Name Term

type Type = Term

data Kind
  = Star
  | Pop
  | Box
  deriving (Show, Read, Ord, Eq, Generic)

data Term
  = Var TName
  | App Term Term
  | Lam (Bind (TName, Embed Annot) Term)
  | Pi  (Bind (TName, Embed Term) Term)

  | Let (Bind (TName, Embed Term) Term)
  | If Term Term Term Annot

  | Lit   Lit
  | TyLit TLit
  | Prim  PrimInstr

  | Dup Term
  | Free TName Term

  | Ann   Term Term
  | Paren Term
  | TLoc  Location Term
  | Hole

  | Kind Kind
  deriving (Show, Generic, Typeable)


newtype Annot = Annot (Maybe Term)
  deriving (Show, Generic, Typeable)

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


-- -----------------------------------------------------------------------------
-- | Pretty Printing

instance PP.Pretty TName where
  pretty = PP.textStrict . pack . name2String

instance PP.Pretty Term where
  pretty t = undefined