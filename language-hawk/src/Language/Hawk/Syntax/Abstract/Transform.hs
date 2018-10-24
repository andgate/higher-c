{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , GADTs
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , ExistentialQuantification
  #-}
module Language.Hawk.Rename where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Set (Set)
import Data.Text (Text, unpack)
import Language.Hawk.Rename.Error
import Language.Hawk.Syntax.Bound
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Prim
import Unbound.Generics.LocallyNameless


import qualified Data.Set as Set
import qualified Language.Hawk.Syntax.Source as S


import qualified Data.List.NonEmpty             as NE


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