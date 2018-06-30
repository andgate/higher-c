{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , PatternSynonyms
  #-}
module Language.Hawk.Syntax.Term.Scoped where


import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable as Foldable
import Data.Default.Class
import Data.Functor.Classes
import Data.Monoid
import Data.Traversable
import Data.Set (Set)
import Data.Text (Text)

import Language.Hawk.Syntax.Definition.Scoped
import Language.Hawk.Syntax.GlobalBind
import Language.Hawk.Syntax.Let
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Pattern
import Language.Hawk.Syntax.Pattern.Source
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Subterm
import Language.Hawk.Syntax.Telescope

import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Set as Set


-- -----------------------------------------------------------------------------
-- | Dependent Scoped Term

data Term v
  = TVar  v
  | TGlobal Text

  | TLit  Lit
  | TCon  Text
  | TPrim PrimInstr (Term v) (Term v)

  | TApp  (Term v) (Term v)
  | TLam  (Pat (PatScope Type v) ()) (PatScope Term v)

  | TPi   (Pat (PatScope Type v) ()) (PatScope Term v)   -- Regular pi, or arrow
  | TLPi  (Pat (PatScope Type v) ()) (PatScope Term v)   -- Linear pi, or lolipop

  | TLet  [(Loc, NameHint, PatDef (Clause LetVar Term v), Maybe (LetScope Type v))] (LetScope Term v)
  | TCase (Term v) [(Pat (PatScope Type v) (), PatScope Term v)]
  
  | TDup  (Term v)
  | TFree [Term v] (Term v)

  -- Hints
  | TAnnot  (Term v) (Type v)
  | TSub Subterm (Term v)
  | TLoc   Loc (Term v)
  | TParen (Term v)

  | TWild

type Type = Term


-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default (Term v) where
  def = TCon "()"


-- -----------------------------------------------------------------------------
-- | Instances

instance Eq1 Term where
  liftEq f t1 t2 = case (t1, t2) of
    (TVar v1, TVar v2)            -> f v1 v2
    (TGlobal g1, TGlobal g2)      -> g1 == g2
    (TLit l1, TLit l2)            -> l1 == l2
    (TCon c1, TCon c2)            -> c1 == c2
    
    (TApp e1 e1', TApp e2 e2') -> liftEq f e1 e2 && liftEq f e1' e2'
    (TLam pat1 s1, TLam pat2 s2) -> liftPatEq (liftEq f) (==) pat1 pat2 && liftEq f s1 s2
    
    (TPi pat1 s1, TPi pat2 s2) -> liftPatEq (liftEq f) (==) pat1 pat2 && liftEq f s1 s2
    (TLPi pat1 s1, TLPi pat2 s2) -> liftPatEq (liftEq f) (==) pat1 pat2 && liftEq f s1 s2    
    
    (TLet tele1 s1, TLet tele2 s2) -> liftEq (\(_, _, d1, mt1) (_, _, d2, mt2) -> liftEq (liftEq f) d1 d2 && liftEq (liftEq f) mt1 mt2) tele1 tele2 && liftEq f s1 s2
    (TCase e1 brs1, TCase e2 brs2) ->
      liftEq f e1 e2
        && liftEq (\(pat1, s1) (pat2, s2) ->
                      liftPatEq (liftEq f) (==) pat1 pat2
                        && liftEq f s1 s2
                  ) brs1 brs2


    (TAnnot _ e1,  e2) -> liftEq f e1 e2
    (e1, TAnnot _ e2) -> liftEq f e1 e2

    (TSub _ e1,  e2) -> liftEq f e1 e2
    (e1, TSub _ e2) -> liftEq f e1 e2
    
    (TLoc _ e1,  e2) -> liftEq f e1 e2
    (e1, TLoc _ e2) -> liftEq f e1 e2
    
    (TWild, TWild) -> True
    _ -> False


instance Eq v => Eq (Term v) where
  (==) = liftEq (==)


instance GlobalBind Term where
  global = TGlobal
  bind f g expr = case expr of
    TVar v -> f v
    TGlobal v -> g v
    TLit l -> TLit l
    TCon c -> TCon c
    TPrim i a b -> TPrim i (bind f g a) (bind f g b)
    
    TApp e1 e2 -> TApp (bind f g e1) (bind f g e2)
    TLam pat s -> TLam (first (bound f g) pat) (bound f g s)
    
    TPi  pat s -> TPi  (first (bound f g) pat) (bound f g s)
    TLPi pat s -> TLPi (first (bound f g) pat) (bound f g s)
    
    TLet tele s -> TLet ((\(loc, h, pd, mt) -> (loc, h, bound f g <$> pd, bound f g <$> mt)) <$> tele) (bound f g s)
    TCase e brs -> TCase (bind f g e) (bimap (first (bound f g)) (bound f g) <$> brs)

    TDup v     -> TDup (bind f g v)
    TFree vs e -> TFree (bind f g <$> vs) (bind f g e)

    TAnnot e ty -> TAnnot (bind f g e) (bind f g ty)
    TSub st e   -> TSub st (bind f g e)
    TLoc l e   -> TLoc l (bind f g e)
    
    TWild -> TWild

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = TVar
  trm >>= f = bind f TGlobal trm

instance Functor Term where fmap = fmapDefault
instance Foldable Term where foldMap = foldMapDefault

instance Traversable Term where
  traverse f trm = case trm of
    TVar v -> TVar <$> f v
    TGlobal v -> pure $ TGlobal v
    
    TLit l -> pure $ TLit l
    TCon c -> pure $ TCon c
    TPrim i a b -> TPrim i <$> traverse f a <*> traverse f b

    TApp e1 e2 -> TApp <$> traverse f e1 <*> traverse f e2
    TLam pat s -> TLam <$> bitraverse (traverse f) pure pat <*> traverse f s
    
    TPi  pat s -> TPi  <$> bitraverse (traverse f) pure pat <*> traverse f s
    TLPi pat s -> TLPi <$> bitraverse (traverse f) pure pat <*> traverse f s
    
    TLet tele s -> TLet <$> traverse (bitraverse (traverse $ traverse f) $ traverse $ traverse f) tele <*> traverse f s
    TCase e brs ->
      TCase <$> traverse f e
            <*> traverse (bitraverse (bitraverse (traverse f) pure) (traverse f)) brs
    
    TLoc l e -> TLoc l <$> traverse f e
    
    TWild -> pure TWild


-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance PP.Pretty v => PP.Pretty (Term v) where
    pretty = \case
      TVar n      -> PP.pretty n
      TGlobal n   -> PP.pretty n

      TLit l      -> PP.pretty l
      TCon n      -> PP.pretty n
      TPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b
      
      TApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      TLam n e    ->
        undefined
        {-
          PP.textStrict "\\" PP.<> PP.pretty n
            PP.<+> PP.textStrict "->"
            PP.<$> PP.indent 2 (PP.pretty e)
        -}

      TPi _ _-> undefined    
      
      TLPi _ _-> undefined    

      TLet xs e ->
          PP.textStrict           "let"
            PP.<+> undefined
            PP.<$> PP.textStrict  "in"
            PP.<+> undefined

      TCase _ _ ->
        undefined

      TDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      TFree vs e ->
          PP.textStrict           "free"
            PP.<$> PP.indent 2 (PP.hsep (PP.pretty <$> vs))
            PP.<$> PP.textStrict  "in"
            PP.<$> PP.indent 2 (PP.pretty e)


      TAnnot e ty ->
          PP.pretty               e
            PP.<+> PP.textStrict ":"
            PP.<+> PP.pretty      ty


      TLoc l e ->
        PP.pretty               e

      TSub ts e ->
        PP.pretty               e
            PP.<+> PP.textStrict "?"
            PP.<+> PP.pretty ts
      
      TParen t    -> PP.parens $ PP.pretty t

      TWild -> PP.textStrict "_"