{-# LANGUAGE  DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , GeneralizedNewtypeDeriving
            , TemplateHaskell
  #-}
module Language.Hawk.Syntax.Term.Sat where

import Bound
import Control.Monad
import Data.Default.Class
import Data.Deriving
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import Language.Hawk.Syntax.Branch
import Language.Hawk.Syntax.GlobalBind
import Language.Hawk.Syntax.Let
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Subterm

import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Set as Set


-- -----------------------------------------------------------------------------
-- | Terms


-- Dependent Term
data Term v
  = TVar  v
  | TGlobal Text
  | TLit  Lit
  | TCon  Text [Term v]
  | TPrim PrimInstr (Term v) (Term v)
  | TLam  NameHint (Type v) (Scope () Term v)
  | TApp  (Term v) (Term v)

  | TLet  (LetRec Term v) (LetScope Term v)
  
  | TCase   (Term v) (Branches Text () Term v)

  | TDup  (Term v)
  | TFree [Term v] (Term v)
  
  | TAnnot (Term v) (Type v)
  deriving(Foldable, Functor, Traversable)

type Type = Term


-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default (Term v) where
  def = TCon "()" []

-- -----------------------------------------------------------------------------
-- | Instances

instance GlobalBind Term where
  global = TGlobal
  bind f g = \case
    TVar v -> f v
    TGlobal v -> g v
    TLit l -> TLit l
    TCon qc es -> TCon qc (bind f g <$> es)
    TPrim i a b -> TPrim i (bind f g a) (bind f g b)
    TLam h e s -> TLam h (bind f g e) (bound f g s)
    TApp e1 e2 -> TApp (bind f g e1) (bind f g e2)
    TLet ds s -> TLet (bound f g ds) (bound f g s)
    TCase e brs -> TCase (bind f g e) (bound f g brs)
    TDup v -> TDup $ bind f g v
    TFree vs e -> TFree (bind f g <$> vs) (bind f g e)
    TAnnot e t -> TAnnot (bind f g e) (bind f g t)


instance Applicative Term where
  pure = TVar
  (<*>) = ap

instance Monad Term where
  return = TVar
  trm >>= f = bind f TGlobal trm

deriveEq1 ''Term
deriveEq ''Term
deriveOrd1 ''Term
deriveOrd ''Term
deriveShow1 ''Term
deriveShow ''Term


-- -----------------------------------------------------------------------------
-- | Pretty Printing

instance PP.Pretty v => PP.Pretty (Term v) where
    pretty = \case
      TVar n      -> PP.pretty n
      TGlobal n   -> PP.pretty n
      TLit l      -> PP.pretty l
      TCon c xs   -> PP.pretty c PP.<+> PP.hsep (PP.pretty <$> xs)
      TPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b

      TLam h e s    ->
        undefined
      
      TApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      
      TLet xs e ->
        undefined
      
      TCase e brs ->
        undefined

      TDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      TFree n e ->
          PP.textStrict           "free"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      TAnnot e ty ->
          PP.pretty               e
            PP.<+> PP.textStrict ":"
            PP.<+> PP.pretty      ty