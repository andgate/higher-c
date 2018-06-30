{-# LANGUAGE  DeriveFunctor
            , DeriveFoldable
            , DeriveTraversable
            , MonadComprehensions
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Pattern.Source where

import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Classes
import Data.Monoid
import Data.Text (Text)
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name

import qualified Text.PrettyPrint.Leijen.Text as PP

------------------------------------------------------------------------------
-- Source Pattern

data Pat t b
  = PVar NameHint b
  | PWild
  | PLit Lit
  | PCon Text [Pat t b]
  | PAnnot (Pat t b) t
  | PView t (Pat t b)
  | PLoc Loc (Pat t b)
  deriving (Foldable, Functor, Show, Traversable)


-------------------------------------------------------------------------------
-- Helpers

liftPatEq :: (typ1 -> typ2 -> Bool) -> (a -> b -> Bool) -> Pat typ1 a -> Pat typ2 b -> Bool
liftPatEq f g pat1 pat2 = case (pat1, pat2) of
    (PVar _ a, PVar _ b)  -> g a b
    (PWild, PWild)        -> True
    (PLit l1, PLit l2)    -> l1 == l2
    (PCon n1 as1, PCon n2 as2) -> n1 == n2 && liftEq (liftPatEq f g) as1 as2
    
    (PAnnot p1 t1, PAnnot p2 t2) -> liftPatEq f g p1 p2 && f t1 t2
    (PView t1 p1, PView t2 p2)   -> f t1 t2 && liftPatEq f g p1 p2
    
    (PLoc _ p1, p2) -> liftPatEq f g p1 p2
    (p1, PLoc _ p2) -> liftPatEq f g p1 p2
    
    _ -> False


------------------------------------------------------------------------------
-- Instances

instance Locatable (Pat typ b) where
  locOf = \case
    PLoc l _ -> l
    -- No other annotations compete with PLoc
    _ -> error "Pattern does not have a location."


instance (Eq typ, Eq b) => Eq (Pat typ b) where
  (==) p1 p2 = case (p1, p2) of
    (PVar h1 b1, PVar h2 b2) -> h1 == h2 && b1 == b2
    (PWild, PWild) -> True
    (PLit l1, PLit l2) -> l1 == l2
    (PCon c1 as1, PCon c2 as2) -> c1 == c2 && as1 == as2
    (PAnnot p1 t1, PAnnot p2 t2) -> t1 == t2 && p1 == p2
    (PView t1 p1, PView t2 p2) -> t1 == t2 && p1 == p2
    (PLoc _ pat1, pat2) -> pat1 == pat2
    (pat1, PLoc _ pat2) -> pat1 == pat2
    _ -> False

instance Applicative (Pat typ) where
  pure = return
  (<*>) = ap

instance Monad (Pat typ) where
  return = PVar mempty
  pat >>= f = case pat of
    PVar h b -> case f b of
      PVar h' b' -> PVar (h' <> h) b'
      fb -> fb
    PWild -> PWild
    PLit l -> PLit l
    PCon c pats -> PCon c [p >>= f | p <- pats]
    PAnnot p t -> PAnnot (p >>= f) t
    PView t p -> PView t $ p >>= f
    PLoc loc p -> PLoc loc $ p >>= f

instance Bifunctor Pat where bimap = bimapDefault
instance Bifoldable Pat where bifoldMap = bifoldMapDefault

instance Bitraversable Pat where
  bitraverse f g pat = case pat of
    PVar h b -> PVar h <$> g b
    PWild -> pure PWild
    PLit l -> pure $ PLit l
    PCon c pats -> PCon c <$> traverse (bitraverse f g) pats
    PAnnot p t -> PAnnot <$> bitraverse f g p <*> f t
    PView t p -> PView <$> f t <*> bitraverse f g p
    PLoc loc p -> PLoc loc <$> bitraverse f g p

------------------------------------------------------------------------------
-- Pretty Printing

instance (PP.Pretty t, PP.Pretty b) => PP.Pretty (Pat t b) where
  pretty = \case
    PVar h n ->
      PP.pretty n

    PWild->
      PP.textStrict "_"
    
    PLit l ->
      PP.pretty l

    PCon n ps ->
      PP.textStrict n PP.<+> PP.pretty ps

    PAnnot p t ->
      PP.pretty p
        PP.<+> PP.textStrict ":"
        PP.<+> PP.pretty t

    PView n p ->
      PP.pretty n
        PP.<> PP.textStrict "@"
        PP.<> PP.pretty p

    PLoc _ p ->
      PP.pretty p -- Usually best to hide locations