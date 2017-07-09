{-# LANGUAGE  DeriveGeneric
            , DeriveFunctor
            , DeriveFoldable
            , DeriveTraversable
            , StandaloneDeriving
            , FlexibleContexts
            , UndecidableInstances
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
  #-}
module Language.Hawk.Syntax.Expression where

import Bound
import Data.Binary (Binary)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Default.Class
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import GHC.Generics (Generic)

-- From deriving-compat
import Data.Eq.Deriving (deriveEq1)
import Data.Ord.Deriving (deriveOrd1)
import Text.Read.Deriving (deriveRead1) 
import Text.Show.Deriving (deriveShow1) 

import Language.Hawk.Syntax.Extensions
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.TypeLiteral

import qualified Text.PrettyPrint.Leijen.Text as PP


-- -----------------------------------------------------------------------------
-- | Expression

data Exp b
  = ELit  Lit
  | EVar  b
  | ECon  Con
  | EPrim PrimInstr
  | EApp  (Exp b) (Exp b)
  | ELam  (Scope () Exp b)
  | EIf   (Exp b) (Exp b) (Exp b)
  | ELet  b (Exp b) (Exp b)
  | EDup  (Exp b)
  | EDrop b (Exp b)

  -- Annotations
  | EType Type (Exp b)
  | ETLit TLit (Exp b)
  | ELoc  Location (Exp b)
  deriving(Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- | Instances


instance Default (Exp b) where
  def = ECon . Con $ "()"

instance Applicative Exp where
  pure  = EVar
  (<*>) = ap

instance Monad Exp where
  return = EVar

  EVar b   >>= f = f b
  EApp x y >>= f = EApp (x >>= f) (y >>= f)
  ELam e   >>= f = ELam (e >>>= f)

deriveEq1 ''Exp
deriveOrd1 ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp

deriving instance (Eq b) => Eq (Exp b)
deriving instance (Ord b) => Ord (Exp b)
deriving instance (Read b) => Read (Exp b)
deriving instance (Show b) => Show (Exp b)

deriving instance (Generic b) => Generic (Exp b)
instance Serial1 Exp where
    serializeWith m = \case
      _ -> undefined
    
    deserializeWith m = getWord8 >>= \case
      _ -> undefined

instance (Generic b, Binary b) => Binary (Exp b)

-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

lam :: Eq b => b -> Exp b -> Exp b
lam v b = ELam (abstract1 v b)

-- -----------------------------------------------------------------------------
-- | Extension Instances

instance PP.Pretty b => PP.Pretty (Exp b) where
    pretty (ELit lit) =
      PP.textStrict "Literal:" PP.<+> PP.pretty lit

    pretty (EVar name) =
      PP.textStrict "Variable:" PP.<+> PP.pretty name
      
    pretty (ECon name) =
      PP.textStrict "Con:" PP.<+> PP.pretty name

    pretty (EPrim i) =
      PP.textStrict "Prim:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "instruction:" PP.<+> PP.pretty i
        )

    pretty (EApp f as) =
      PP.textStrict "Application:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "expression:" PP.<+> PP.pretty f
          PP.<$>
          PP.textStrict "applied to:" PP.<+> PP.pretty as
        )

    pretty (ELam e) =
      PP.textStrict "Lambda:"
      {-- 
        PP.<$>
        PP.indent 2
        ( -- Existence is sufferring
          PP.textStrict "body:" PP.<+> PP.pretty e
        )
      --}

    pretty (EIf predicate thenBranch elseBranch) =
      PP.textStrict "If:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "predicate:" PP.<+> PP.pretty predicate
          PP.<$>
          PP.textStrict "then branch:" PP.<+> PP.pretty thenBranch
          PP.<$>
          PP.textStrict "else branch:" PP.<+> PP.pretty elseBranch
        )

    pretty (ELet n lhs e) =
      PP.textStrict "Let:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "name:" PP.<+> PP.pretty n
          PP.<$>
          PP.textStrict "lhs:" PP.<+> PP.pretty lhs
          PP.<$>
          PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (EDup e) =
      PP.textStrict "Dup:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (EDrop n e) =
      PP.textStrict "Drop:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "var:" PP.<+> PP.pretty n
          PP.<$>
          PP.textStrict "in:" PP.<+> PP.pretty e
        )
        
    pretty (EType e t) =
      PP.textStrict "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "expression:" PP.<+> PP.pretty e
          PP.<$>
          PP.textStrict "hint:" PP.<+> PP.pretty t
        )