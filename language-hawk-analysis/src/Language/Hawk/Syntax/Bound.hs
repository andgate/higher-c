{-# Language  DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses
  #-}
module Language.Hawk.Syntax.Bound where

import GHC.Generics
import Data.Typeable (Typeable)
import Unbound.Generics.LocallyNameless

import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Location


type Var = Name Term

type Type = Term

data Term
  = Type
  | Linear
  | TVar Var
  | TVal PrimVal
  | TCon String
  | TPrim PrimInstr Term Term

  | TApp Term [Term]
  | TLam (Bind Tele Term)
  | TPi (Bind TeleP Term)
  | TSigma (Bind Tele Term)
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)

  | TAnn Term Term
  | TLoc Loc Term
  | TWild
  deriving(Show, Generic, Typeable)


data Tele
  = ScopeNil
  | ScopeCons (Rebind (Var, Embed Term) Tele)
  deriving (Show, Generic, Typeable)

data TeleP
  = ScopeNilP
  | ScopeConsP (Rebind (Var, Embed Term, Plicity) TeleP)
  deriving (Show, Generic, Typeable)


data Plicity = Implicit | Explicit
  deriving (Show, Generic, Typeable)


--------------------------------------------------------------------------------------------------
-- Smart Constructors

tvar :: String -> Term
tvar = TVar . string2Name

mkTele :: [(String, Term)] -> Tele
mkTele []           = ScopeNil
mkTele ((v, b) : t) = ScopeCons (rebind (string2Name v, embed b) $ mkTele t)

mkTeleP :: [(String, Term, Plicity)] -> TeleP
mkTeleP []              = ScopeNilP
mkTeleP ((v, b, p) : t) = ScopeConsP (rebind (string2Name v, embed b, p) $ mkTeleP t)


tlam :: [(String, Term)] -> Term -> Term
tlam t b = TLam (bind (mkTele t) b)


tpi :: [(String, Term, Plicity)] -> Term -> Term
tpi t b = TPi $ bind (mkTeleP t) b


tarr :: Term -> Term -> Term
tarr t1 t2 = tpi [("_", t1, Explicit)] t2

tapp :: Term -> Term -> Term
tapp f x = TApp f [x]

tapps :: Term -> [Term] -> Term
tapps = TApp


tsigma :: [(String, Term)] -> Term -> Term
tsigma [] b = b
tsigma t b = TSigma $ bind (mkTele t) b


tlet :: [(String, Term)] -> Term -> Term
tlet bs b = 
  let bs' = rec [ (string2Name v, embed t) | (v, t) <- bs]
  in TLet $ bind bs' b


------------------------------------------------------------------------------------------------------
-- Instances Required for unbound-generics

instance Alpha Term
instance Alpha Tele
instance Alpha TeleP


instance Alpha Loc where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Region where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Position where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha PrimInstr where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha PrimVal where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Plicity where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ



instance Subst Term Loc
instance Subst Term Region
instance Subst Term Position
instance Subst Term PrimInstr
instance Subst Term PrimVal
instance Subst Term Plicity
instance Subst Term Tele
instance Subst Term TeleP
instance Subst Term Term where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing