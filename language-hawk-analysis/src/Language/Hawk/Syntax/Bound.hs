{-# Language DeriveDataTypeable
           , DeriveGeneric
           , MultiParamTypeClasses
           , OverloadedStrings
           , LambdaCase
           , RankNTypes
  #-}
module Language.Hawk.Syntax.Bound where

import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics
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

untele :: forall m. Fresh m => Tele -> m [(Var, Term)]
untele = \case
  ScopeNil     -> return []
  ScopeCons rb -> do
    let ((v, Embed t), tele) = unrebind rb
    tele' <- untele tele
    return $ (v, t) : tele'


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


instance Pretty Term where
  pretty = runFreshM . prettyFresh


prettyFresh :: Term -> FreshM (Doc ann)
prettyFresh = \case
  Type   -> return "Type"
  Linear -> return "Linear"
  
  TVar v        -> return . pretty . show $ v
  TVal v        -> return $ pretty v
  TCon c        -> return $ pretty c
  TPrim i t1 t2 -> do
    t1' <- prettyFresh t1
    t2' <- prettyFresh t2
    return $ pretty i <+> parens t1' <+> parens t2'
  
  TApp f as -> do
    f' <- prettyFresh f 
    as' <- mapM prettyFresh as
    return $ f' <+> hcat as'

  TLam bnd -> do
    (t, body) <- unbind bnd
    t' <- prettyFreshTele t
    body' <- prettyFresh body
    return $ "\\" <+> t' <+> "." <+> body'

  TPi bnd -> do
    (t, body) <- unbind bnd
    t' <- prettyFreshTeleP t
    body' <- prettyFresh body
    return $ "\\" <+> t' <+> "->" <+> body'

  TSigma bnd -> do
    (t, body) <- unbind bnd
    t' <- prettyFreshSigma t
    body' <- prettyFresh body
    return $ tupled (t' ++ [body'])

  TLet bnd -> undefined
  
  TAnn tm ty -> do
    tm' <- prettyFresh tm
    ty' <- prettyFresh ty
    return $ tm' <+> ":" <+> ty'

  TLoc _ t -> prettyFresh t
  TWild -> return "_"


prettyFreshTele :: Tele -> FreshM (Doc ann)
prettyFreshTele = \case
  ScopeNil     -> return emptyDoc
  ScopeCons rb -> do
    let ((x, Embed t), tele) = unrebind rb
        x' = pretty $ show x
    t'    <- prettyFresh t 
    tele' <- prettyFreshTele tele
    return $ parens (x' <+> t') <+> tele'


prettyFreshTeleP :: TeleP -> FreshM (Doc ann)
prettyFreshTeleP = \case
  ScopeNilP     -> return emptyDoc
  ScopeConsP rb -> do
    let ((x, Embed t, pl), tele) = unrebind rb
        x' = pretty $ show x
    t'    <- prettyFresh t 
    tele' <- prettyFreshTeleP tele
    case pl of
      Explicit -> return $ parens (x' <+> t') <+> tele'
      Implicit -> return $ "@" <+> parens (x' <+> t') <+> tele'

prettyFreshSigma :: Tele -> FreshM [Doc ann]
prettyFreshSigma = \case
  ScopeNil     -> return [] 
  ScopeCons rb -> do
    let ((x, Embed t), tele) = unrebind rb
        x' = pretty $ show x
    t'    <- prettyFresh t 
    tele' <- prettyFreshSigma tele
    return $ (x' <+> t') : tele'