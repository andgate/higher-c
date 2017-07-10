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
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1) 
import Data.List (elemIndex)
import Data.Foldable hiding (notElem)
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import GHC.Generics (Generic)

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
  = EVar  b
  | EApp  (Exp b) (Exp b)
  | ELam  b (Exp b)
  | ELet  (b, Exp b) (Exp b)
  | ELit  Lit
  | ECon  Con
  | EPrim PrimInstr
  | EIf   (Exp b) (Exp b) (Exp b)
  | EDup  (Exp b)
  | EFree b (Exp b)

  -- Hints
  | EType Type (Exp b)
  | ETLit TLit (Exp b)
  | ELoc  Location (Exp b)
  deriving(Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- | Instances


instance Default (Exp b) where
  def = ECon . Con $ "()"

{-
instance Applicative Exp where
  pure  = EVar
  (<*>) = ap

instance Monad Exp where
  return = EVar

  EVar b    >>= f   = f b
  EApp x y  >>= f   = EApp (x >>= f) (y >>= f)
  ELam e    >>= f   = ELam (e >>>= f)
  ELet bs b >>= f   = ELet (map (>>>= f) bs) (b >>>= f)
  ELit l    >>= _   = ELit l
  ECon c    >>= _   = ECon c
  EPrim i   >>= _   = EPrim i
  EIf x a b >>= f   = EIf (x >>= f) (a >>= f) (b >>= f)
  EDup e    >>= f   = EDup (e >>= f)
  EDrop b e >>= f   = EDrop (b >>>= f) (e >>= f)
  EType t e >>= f   = EType t (e >>= f)
  ETLit t e >>= f   = ETLit t (e >>= f)
  ELoc lc e >>= f   = ELoc lc (e >>= f)

-}

deriving instance (Eq b) => Eq (Exp b)
deriving instance (Ord b) => Ord (Exp b)
deriving instance (Read b) => Read (Exp b)
deriving instance (Show b) => Show (Exp b)

{-
deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp
-}


deriving instance (Generic b) => Generic (Exp b)
instance (Generic b, Binary b) => Binary (Exp b)
{-
instance Serial1 Exp where
    serializeWith m = \case
      _ -> undefined
    
    deserializeWith m = getWord8 >>= \case
      _ -> undefined
-}


-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

{-
lam :: Eq a => a -> Exp a -> Exp a
lam v b = ELam (abstract1 v b)

let_ :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
let_ [] b = b
let_ bs b = ELet (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

-}

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

    pretty (ELam b e) =
      PP.textStrict "Lambda:" 
      PP.<$>
      PP.indent 2
      (
        PP.textStrict "binding:" PP.<+> PP.pretty b
        PP.<$>
        PP.textStrict "expression:" PP.<+> PP.pretty e
      )

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

    
    pretty (ELet bs e) =
      PP.textStrict "Let:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "bindings:" PP.<+> PP.pretty bs
          PP.<$>
          PP.textStrict "in:" PP.<+> PP.pretty e
        )

    pretty (EDup e) =
      PP.textStrict "Dup:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (EFree n e) =
      PP.textStrict "Free:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "var:" PP.<+> PP.pretty n
          PP.<$>
          PP.textStrict "so:" PP.<+> PP.pretty e
        )
        
    pretty (EType e t) =
      PP.textStrict "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "expression:" PP.<+> PP.pretty e
          PP.<$>
          PP.textStrict "type:" PP.<+> PP.pretty t
        )

    pretty (ETLit t e) =
      PP.textStrict "TLit Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "tlit:" PP.<+> PP.pretty t
          PP.<$>
          PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (ELoc loc e) =
      PP.textStrict "Location Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "location:" PP.<+> PP.pretty loc
          PP.<$>
          PP.textStrict "exp:" PP.<+> PP.pretty e
        )