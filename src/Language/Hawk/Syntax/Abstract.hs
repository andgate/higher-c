{-# Language DeriveDataTypeable
           , DeriveGeneric
           , MultiParamTypeClasses
           , OverloadedStrings
           , LambdaCase
           , RankNTypes
  #-}
module Language.Hawk.Syntax.Abstract where

import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import Language.Hawk.Syntax.Builtin
import Language.Hawk.Syntax.Location


import qualified Data.List.NonEmpty            as NE
import qualified Data.Set                      as Set
import qualified Language.Hawk.Syntax.Concrete as C


{-
data Module
  = Module
    { modSubs  :: [Module]
    , modNames :: [Text]
    , modFns   :: Bind (Rec [(Var, Embed Term)]) Term
    }
-}

type Var = Name Exp
type TVar = Name Type


data Func
  = Func Text (Bind [(Var, Embed (Maybe Type))] Stmt)
  deriving (Show, Generic, Typeable)

data Stmt
  = SCall Exp [Exp] Stmt
  | SLet (Bind (Var, Embed (Maybe Type, Maybe Exp)) Stmt)
  | SAssign Exp (Maybe Type) Exp Stmt
  | SBlock Stmt

  -- Terminal Statements
  | SReturn Exp
  | SEmpty

  -- Annotations
  | SLoc Loc Stmt
  deriving (Show, Generic, Typeable)


data Exp
  = EVar Var
  | ECon Text
  | EVal Val
  | EInstr Instr Exp Exp
  | ECall Exp [Exp]
  | EType Exp Type
  | ELoc  Loc Exp
  deriving (Show, Generic, Typeable)

data Type
  = TVar TVar
  | TCon Text
  | TArr Type Type
  | TApp Type Type
  | TForall (Bind [TVar] Type)
  | TKind Type Kind
  | TLoc Loc Type
  deriving (Show, Generic, Typeable)

data Kind
  = KType
  | KArr Kind Kind
  | KLoc Loc Kind
  deriving (Show, Generic, Typeable)


--------------------------------------------------------------------------------------------------
-- Smart Constructors

{-
evar :: String -> Exp
evar = EVar . string2Name

tvar :: String -> Type
tvar = TVar . string2Name

slet :: (String, Maybe Type, Maybe Exp) -> Stmt -> Stmt
slet (v, ty, e) b =
  let bs' = (string2Name v, embed (ty, e))
  in SLet $ bind bs' b
-}

{-
free :: Term -> [Text]
free t =
  (pack . name2String) <$> toListOf (fv :: Fold Term (Name Term)) t
-}

------------------------------------------------------------------------------------------------------
-- Instances Required for unbound-generics
{-
instance Alpha Type
instance Alpha Exp
instance Alpha Func
instance Alpha Stmt


instance Alpha Loc where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Region where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Position where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Instr where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Val where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ


instance Subst Exp Loc
instance Subst Exp Region
instance Subst Exp Position
instance Subst Exp Instr
instance Subst Exp Val
instance Subst Exp Exp where
  isvar (EVar x) = Just (SubstName x)
  isvar _ = Nothing


instance Pretty Exp where
  pretty = runFreshM . prettyFreshExp


prettyFreshExp :: Exp -> FreshM (Doc ann)
prettyFreshExp = \case
  EVar v        -> return . pretty . show $ v
  EVal v        -> return $ pretty v
  ECon c        -> return $ pretty c
  EInstr i t1 t2 -> do
    t1' <- prettyFreshExp t1
    t2' <- prettyFresh t2
    return $ pretty i <+> parens t1' <+> parens t2'
-}
{-
  TApp f as -> do
    f' <- prettyFreshExp f
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

-}

------------------------------------------------------------------------------------------------------
-- Abstracting a concrete term

{-

data AbstractError
  = UndeclaredName Loc Text
    deriving(Show)

instance Pretty AbstractError where
    pretty = \case
        UndeclaredName n l ->
           pretty l <> ":Undeclared Name:" <+> pretty n


data REnv = REnv
  { _rnLoc   :: Loc
  , _rnNames :: Set String
  }

newtype Renamer a = Renamer { unRenamer :: ReaderT REnv (Except RenameError) a }
  deriving (Functor, Applicative, Monad, MonadReader REnv, MonadError RenameError)


rename :: [String] -> S.Term -> Either RenameError Term
rename gs t = runRenamer (Set.fromList gs) $ renameTerm t


runRenamer :: Scope -> Renamer a -> Either RenameError a
runRenamer gs rn = runExcept $ runReaderT (unRenamer rn) gs



renameTerm :: S.Term -> Renamer Term
renameTerm = \case
  S.Type      -> return $ TUni Type
  S.Linear    -> return $ TUni Linear

  S.TVar n    -> do
    return $ tvar (unpack n)

  S.TCon n    -> do
    let n' = unpack n
    q <- reader (Set.member n')
    if q
      then return $ TCon n'
      else throwError $ UndefinedName n

  S.TVal v    -> return $ TVal v
  
  S.TPrim i t1 t2 -> TPrim i <$> renameTerm t1 <*> renameTerm t2

  S.TApp f as -> 
    tapps <$> renameTerm f <*> traverse renameTerm (NE.toList as)

  S.TLam ps body -> do
    ps' <- traverse renamePat (NE.toList ps)
    let vs = fst <$> ps'
    body' <- local (\env -> foldr Set.insert env vs) $ renameTerm body
    return $ tlam ps' body'

  S.TPi ps body ->
    tpi <$> traverse renamePlicitPat (NE.toList ps) <*> renameTerm body

  S.TSigma ps body ->
    tsigma <$> traverse renamePat ps <*> renameTerm body

  S.TLet bs body ->
    tlet <$> traverse renamePatBind (NE.toList bs) <*> renameTerm body

  S.TAnn tm ty -> TAnn   <$> renameTerm tm <*> renameTerm ty
  S.TLoc l t   -> TLoc l <$> renameTerm t
  S.TWild      -> return TWild



renamePlicity :: S.Plicity -> Plicity
renamePlicity = \case
  S.Implicit -> Implicit
  S.Explicit -> Explicit


renamePat :: S.Pat -> Renamer (String, Term)
renamePat p = do
  let (v, t) = S.pfree p
  t' <- renameTerm t
  return (unpack v, t')


renamePlicitPat :: S.PlicitPat -> Renamer (String, Term, Plicity)
renamePlicitPat (S.PlicitPat l pl pt) = do
  (v, t) <- renamePat pt
  return (v, t, renamePlicity pl)

renamePatBind :: S.PatBind -> Renamer (String, Term)
renamePatBind (S.PatBind p t) = do
  (v, ty) <- renamePat p
  t'      <- renameTerm t
  return (v, TAnn t' ty)


-}
