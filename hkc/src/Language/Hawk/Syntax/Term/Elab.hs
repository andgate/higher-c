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
module Language.Hawk.Syntax.Term.Elab where

import Bound
import Control.Monad
import Data.Default.Class
import Data.Deriving
import Data.Functor.Classes
import Data.Monoid hiding (Alt)
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

type TermName v = Name (Term v) Text

type Type = Term

-- Dependent Term
data Term v
  = TVar  v
  | TGlobal Text
  | TCon  Text
  | TLit  Lit
  | TPrim PrimInstr (Term v) (Term v)
  | TApp  (Term v) (Term v)
  | TLam  NameHint (Type v) (Scope () Term v)

  | TPi   NameHint (Type v) (Scope () Term v)  -- Regular pi, or arrow
  | TLPi  NameHint (Type v) (Scope () Term v)  -- Linear pi, or lolipop

  | TLet  (LetRec Term v) (LetScope Term v)
  
  | TCase (Term v) (Branches Text () Term v) (Type v)
  
  | TDup  (Term v)
  | TFree [Term v] (Term v)

  | TSub Subterm (Term v)  
  deriving(Foldable, Functor, Traversable)




-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default (Term v) where
  def = TCon "()"


-- -----------------------------------------------------------------------------
-- | Instances


instance GlobalBind Term where
  global = TGlobal
  bind f g trm = case trm of
    TVar v -> f v
    TGlobal v -> g v
    TCon c -> TCon c
    TLit l -> TLit l
    TPrim i a b -> TPrim i (bind f g a) (bind f g b)
    TApp e1 e2 -> TApp (bind f g e1) (bind f g e2)
    TLam h t s -> TLam h (bind f g t) (bound f g s)
    TPi  h t s -> TPi  h (bind f g t) (bound f g s)
    TLPi h t s -> TLPi h (bind f g t) (bound f g s)
    TLet ds scope -> TLet (bound f g ds) (bound f g scope)
    TCase e brs retType -> TCase (bind f g e) (bound f g brs) (bind f g retType)
    TDup e -> TDup (bind f g e)
    TFree vs e -> TFree (bind f g <$> vs) (bind f g e)
    TSub st e -> TSub st (bind f g e)


deriveEq1 ''Term
deriveEq ''Term
deriveOrd1 ''Term
deriveOrd ''Term
deriveShow1 ''Term
deriveShow ''Term


instance Applicative Term where
  pure = return
  (<*>) = ap


instance Monad Term where
  return = TVar
  t >>= f = bind f TGlobal t


-- -----------------------------------------------------------------------------
-- | Pretty Printing

instance PP.Pretty v => PP.Pretty (Term v) where
    pretty = \case
      TVar n      -> PP.pretty n
      TGlobal n   -> PP.pretty n
      TLit l      -> PP.pretty l
      TCon n      -> PP.pretty n
      TPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b
      
      TApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2

      TLam h ty s    ->
        undefined

      TPi h ty s   ->
        undefined

      TLPi h ty s    ->
        undefined
      
      TLet xs e ->
        undefined
      
      TCase e br d ->
        undefined

      TDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      TFree n e ->
          PP.textStrict           "free"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      TSub st e ->
          PP.pretty               e
            PP.<+> PP.textStrict "?"
            PP.<+> PP.pretty      st

