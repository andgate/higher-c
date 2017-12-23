{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Term.Source where

import Control.Arrow (first, second)
import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import Data.Data.Lens (uniplate)
import Data.Default.Class
import Data.Monoid hiding (Alt)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Subterm

import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Set as Set


-- -----------------------------------------------------------------------------
-- | Terms

type TermName = Name Term Text

type Type = Term

-- Dependent Term
data Term v
  = TVar  v
  | TApp  (Term v) (Term v)
  | TLam  Name (Term v)

  | TPi   (Name, Term v) (Term v)    -- Regular pi, or arrow
  | TLPi   (Name, Term v) (Term v)   -- Linear pi, or lolipop

  | TLet  (Name, Term v) (Term v)
  
  | TLit  Lit
  | TCon  Text
  | TPrim PrimInstr (Term v) (Term v)
  | TIf   (Term v) (Term v) (Term v)
  
  | TDup  v
  | TFree [v] (Term v)

  -- Hints
  | THint  (Term v) (Term v)
  | TSub SubTerm (Term v)
  | TLoc   Loc (Term v)
  | TParen (Term v)

  | TWild
  
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)


-- Term instances
instance Binary Term
instance Plated Term
instance FromJSON Term
instance ToJSON Term

-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default Term where
  def = TCon "()"

instance Default Pat where
  def = PWild

-- -----------------------------------------------------------------------------
-- | Term Helpers

-- Remove locations from a term
unlocate :: Term -> Term
unlocate = transform $ \case
  TLoc _ t -> t
  t -> t


-- Remove types from a term
untype :: Term -> Term
untype = transform $ \case
  THint _ t -> t
  t -> t


-- Locations
locTerm :: Term -> Loc
locTerm = \case
  TVar _ -> error "Cannot locate term without location!"
  TApp a b -> locTerm a <> locTerm b
  TLam n e -> locName' n <> locTerm e
  TLet (n, _) e -> locName' n <> locTerm e
  TLit _ -> error "Cannot locate term without location!"
  TCon _ -> error "Cannot locate term without location!"
  TPrim _ a b -> locTerm a <> locTerm b
  TIf a _ b -> locTerm a <> locTerm b
  TDup n -> error "Cannot locate term without location!"
  TFree _ t -> locTerm t
  THint t' t -> locTerm t <> locTerm t'
  TSub c t -> locTerm t
  TLoc l _ -> l
  TParen e -> locTerm e


locPat :: Pat -> Loc
locPat = \case
  PVar n -> error "No location found in pattern."
  PLit l -> error "No location found in pattern."
  PWild-> error "No location found in pattern."
  PAs n p -> locPat p
  PCon n ps -> mconcat (locPat <$> ps)
  PParen p -> locPat p
  PLoc l _ -> l
  PHint t p -> locPat p <> locTerm t


-- Names
termNames :: Term -> [Text]
termNames = \case
  TVar n -> [n]
  _ -> undefined


patNames :: Pat -> [Text]
patNames = \case
  PVar n -> [n]
  PLit l -> []
  PWild-> []
  PAs n p -> n : patNames p
  PCon n ps -> n : concatMap patNames ps
  PParen p -> patNames p
  PLoc _ p -> patNames p 
  PHint t p -> patNames p
  

--------------------------------------------------------------------------------
-- Name Helpers

varName :: Name -> Term
varName = \case
  Name n -> TVar n
  NLoc l n -> TLoc l $ varName n
  NTerm t n -> THint t $ varName n


-- -----------------------------------------------------------------------------
-- | Free Variables

class HasFreeVars a where
  fv :: a -> Set Text


instance HasFreeVars Term where
  fv = \case
    _ -> undefined


instance HasFreeVars Pat where
  fv = \case
    PVar n -> Set.singleton n
    PLit l -> Set.empty
    PWild-> Set.empty
    PAs n p -> Set.singleton n `Set.union` fv p
    PCon n ps -> Set.unions (fv <$> ps)
    PParen p -> fv p
    PLoc _ p -> fv p 
    PHint t p -> fv p


instance HasFreeVars a => HasFreeVars [a] where
  fv = mconcat . map fv


-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance PP.Pretty Term where
    pretty = \case
      TVar n      -> PP.pretty n
      TApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      TLam n e    ->
          PP.textStrict "\\" PP.<> PP.pretty n
            PP.<+> PP.textStrict "->"
            PP.<$> PP.indent 2 (PP.pretty e)
      TLet xs e ->
          PP.textStrict           "let"
            PP.<+> PP.pretty       xs
            PP.<$> PP.textStrict  "in"
            PP.<+> PP.pretty       e
      TLit l      -> PP.pretty l
      TCon n      -> PP.pretty n
      TPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b
      TIf e1 e2 e3 ->
          PP.textStrict           "if"
            PP.<+> PP.pretty       e1
            PP.<+> PP.textStrict  "then"
            PP.<+> PP.pretty       e2
            PP.<+> PP.textStrict  "else"
            PP.<+> PP.pretty       e3

      TDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      TFree n e ->
          PP.textStrict           "free"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      THint t e ->
          PP.pretty               e
            PP.<+> PP.textStrict "::"
            PP.<+> PP.pretty      t


      TLoc l e ->
        PP.pretty               e
            PP.<+> PP.textStrict "@"
            PP.<+> PP.parens (PP.pretty l)

      
      TParen t    -> PP.parens $ PP.pretty t
