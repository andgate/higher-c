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
import Control.Monad.State
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map.Strict (Map)
import Data.Set
import Data.Text (Text, unpack)
import Language.Hawk.Rename.Error
import Language.Hawk.Syntax.Prim
import Unbound.Generics.LocallyNameless


import qualified Data.Map.Strict as Map
import qualified Language.Hawk.Syntax.Source as S

import Language.Hawk.Syntax.Bound

import qualified Data.List.NonEmpty             as NE


type Globals = [Text]

rename :: Globals -> S.Term -> Either RenameError Term
rename gs = runRenamer gs . renameTerm


newtype Renamer a = Rn { unRn :: ReaderT Globals (Except RenameError) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Globals
           , MonadError RenameError
           )


runRenamer :: Globals -> Renamer a -> Either RenameError a  
runRenamer gs rn = runExcept $ runReaderT (unRn rn) gs


renameTerm :: S.Term -> Renamer Term
renameTerm = \case
  S.Type      -> return Type
  S.Linear    -> return Linear

  S.TVar n    -> return $ tvar (unpack n)
  S.TCon n    -> return $ TCon (unpack n)
  S.TVal v    -> return $ TVal v
  
  S.TPrim i t1 t2 -> TPrim i <$> renameTerm t1 <*> renameTerm t2

  S.TApp f as -> 
    tapps <$> renameTerm f <*> traverse renameTerm (NE.toList as)

  S.TLam ps body ->
    tlam <$> traverse renamePat (NE.toList ps) <*> renameTerm body

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