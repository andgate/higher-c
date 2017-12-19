{-# LANGUAGE  LambdaCase
  #-}
module Language.Hawk.TypeCheck.Substitution where

import Data.Text (Text)
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Set (Set)
import Language.Hawk.Syntax
import Language.Hawk.TypeCheck.Constraint

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-----------------------------------------------------------------------
-- Substition Map
-----------------------------------------------------------------------

newtype Subst = Subst (Map Text Type)
  deriving (Eq, Ord, Show)

-- | The empty substitution
empty :: Subst
empty = mempty


-- | Compose substitutions
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) =
  Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

fromList :: [(Text, Type)] -> Subst
fromList = Subst . Map.fromList


instance Default Subst where
  def = Subst Map.empty


instance Monoid Subst where
  mempty = def
  mappend = compose

-----------------------------------------------------------------------
-- Substitutable
-----------------------------------------------------------------------

class Substitutable a where
  apply :: Subst -> a -> a


instance Substitutable Text where
  apply (Subst s) a = tv
    where t = TVar a
          (TVar tv) = Map.findWithDefault t a s

instance Substitutable Fn where
  apply s@(Subst smap) (Fn n ps e) =
    Fn n (apply s <$> ps) (apply s e)

instance Substitutable Pat where
  apply s@(Subst smap) = \case
    PVar n -> PVar n
    PLit lt -> PLit lt
    PWild -> PWild
    PAs n p -> PAs n (apply s p)
    PCon n ps -> PCon n (apply s <$> ps)
    PParen p -> PParen $ apply s p
    PLoc l p -> PLoc l $ apply s p
    PType t p -> PType (apply s t) (apply s p)


instance Substitutable Exp where
  apply s@(Subst smap) = \case
    EApp e1 e2          -> EApp (apply s e1) (apply s e2)
    ELam n e            -> ELam n (apply s e)
    ELet (n, a) b       -> ELet (n, apply s a) (apply s b)
    EIf e1 e2 e3        -> EIf (apply s e1) (apply s e2) (apply s e3)
    EDup n              -> EDup n
    EFree ns e          -> EFree ns (apply s e)
    EType t e           -> EType (apply s t) (apply s e)
    ELoc l e            -> ELoc l (apply s e)
    EParen e            -> EParen (apply s e)

    e -> e -- Cases without nested expressions can be returned


instance Substitutable Type where
  apply s@(Subst s_map) = \case
    t@(TVar a)   -> Map.findWithDefault t a s_map
    TCon n       -> TCon n
    TApp t1 t2   -> apply s t1 `TApp` apply s t2
    TArr t1 t2   -> apply s t1 `TArr` apply s t2
    TLoli t1 t2  -> apply s t1 `TLoli` apply s t2
    TKind k t    -> TKind k $ apply s t
    TLoc l t     -> TLoc l $ apply s t
    TParen t     -> TParen $ apply s t
    TForall tv t -> let s' = Subst $ foldr Map.delete s_map tv
                    in  TForall tv $ apply s' t


instance Substitutable Constraint where
  apply s = \case
    EqConst t1 t2          -> EqConst (apply s t1) (apply s t2)
    ExpInstConst t sc      -> ExpInstConst (apply s t) (apply s sc)
    ImpInstConst t1 ms t2  -> ImpInstConst (apply s t1) (apply s ms) (apply s t2)


instance Substitutable a => Substitutable [a] where
  apply = map . apply


instance (Ord a, Substitutable a) => Substitutable (Set a) where
  apply = Set.map . apply
