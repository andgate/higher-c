{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Expression where

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
import Language.Hawk.Syntax.Pattern
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Kind

import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Set as Set


-- -----------------------------------------------------------------------------
-- | Expression

data Exp
  = EVar  Text
  | EApp  Exp Exp
  | ELam  Name Exp
  | ELet  (Name, Exp) Exp
  | ELit  Lit
  | ECon  Text
  | EPrim PrimInstr Exp Exp
  | EIf   Exp Exp Exp
  | EDup  Name
  | EFree [Name] Exp

  -- Hints
  | EType  Type Exp
  | ELoc   Loc Exp
  | EParen Exp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)


data Binder = Binder Pat Exp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)



class HasVar a where
  var :: a -> Maybe Text

instance HasVar Exp where
  var = \case
    EVar n -> Just n
    ECon n -> Just n
    _      -> Nothing


instance HasType Exp where
  typeof = \case
    EType t _  -> t
    ELoc _ e   -> typeof e
    EParen e   -> typeof e

    --ELit l -> typeof l
    --EPrim i -> typeof i
    
    _ -> error "Cannot find type of untyped expression."


instance HasKind Exp where
  kind = \case
    EType t _ -> kind t
    ELoc _ e -> kind e
    EParen e -> kind e
    _ -> error "no kind"


instance HasKind Binder where
  kind (Binder p e) = kind p


-- -----------------------------------------------------------------------------
-- | Instances

instance Default Exp where
  def = ECon "()"


instance Binary Exp
instance Plated Exp
instance FromJSON Exp
instance ToJSON Exp

instance Binary Binder
instance Plated Binder
instance FromJSON Binder
instance ToJSON Binder

-- -----------------------------------------------------------------------------
-- | "Smart" Constructors


unlocate :: Exp -> Exp
unlocate = transform $ \case
  ELoc _ e -> e
  e -> e
  

untype :: Exp -> Exp
untype = transform $ \case
  EType _ e -> e
  e -> e


-- -----------------------------------------------------------------------------
-- | Expression Manipulations


locExp :: Exp -> Loc
locExp = \case
  EVar _ -> error "Cannot locate expression without location!"
  EApp a b -> locExp a <> locExp b
  ELam n e -> locName' n <> locExp e
  ELet (n, _) e -> locName' n <> locExp e
  ELit _ -> error "Cannot locate expression without location!"
  ECon _ -> error "Cannot locate expression without location!"
  EPrim _ a b -> locExp a <> locExp b
  EIf a _ b -> locExp a <> locExp b
  EDup n -> locName' n
  EFree _ e -> locExp e
  EType t e -> locExp e <> locType t
  ELoc l _ -> l
  EParen e -> locExp e
  


varName :: Name -> Exp
varName = \case
  Name n -> EVar n
  NLoc l n -> ELoc l $ varName n
  NType t n -> EType t $ varName n


-- -----------------------------------------------------------------------------
-- | Helpers


class HasFreeVars a where
  fv :: a -> Set Text


instance HasFreeVars Exp where
  fv = \case
    _ -> undefined


instance HasFreeVars Binder where
  fv (Binder p e) =
    let bound = fv p
        vars = fv e
    in vars `Set.difference` bound


instance HasFreeVars Pat where
  fv = \case
    PVar x -> Set.singleton x
    


-- -----------------------------------------------------------------------------
-- | Class Instances

instance PP.Pretty Exp where
    pretty = \case
      EVar n      -> PP.pretty n
      EApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      ELam n e    ->
          PP.textStrict "\\" PP.<> PP.pretty n
            PP.<+> PP.textStrict "->"
            PP.<$> PP.indent 2 (PP.pretty e)
      ELet xs e ->
          PP.textStrict           "let"
            PP.<+> PP.pretty       xs
            PP.<$> PP.textStrict  "in"
            PP.<+> PP.pretty       e
      ELit l      -> PP.pretty l
      ECon n      -> PP.pretty n
      EPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b
      EIf e1 e2 e3 ->
          PP.textStrict           "if"
            PP.<+> PP.pretty       e1
            PP.<+> PP.textStrict  "then"
            PP.<+> PP.pretty       e2
            PP.<+> PP.textStrict  "else"
            PP.<+> PP.pretty       e3

      EDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      EFree n e ->
          PP.textStrict           "free"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      EType t e ->
          PP.pretty               e
            PP.<+> PP.textStrict "::"
            PP.<+> PP.pretty      t


      ELoc l e ->
        PP.pretty               e
            PP.<+> PP.textStrict "@"
            PP.<+> PP.parens (PP.pretty l)

      
      EParen e    -> PP.parens $ PP.pretty e



instance PP.Pretty Binder where
  pretty (Binder p e) =
    PP.pretty p
      PP.<+> PP.textStrict "="
      PP.<+> PP.pretty e

