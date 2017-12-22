{-# LANGUAGE  LambdaCase
            , FlexibleContexts
  #-}
module Language.Hawk.ScopeCheck where

import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control

import Data.Bag
import Data.Default.Class
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)

import Language.Hawk.ScopeCheck.Environment (Env)
import Language.Hawk.ScopeCheck.Error
import Language.Hawk.ScopeCheck.Message
import Language.Hawk.ScopeCheck.State
import Language.Hawk.Syntax

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Language.Hawk.ScopeCheck.Environment as Env
import qualified Language.Hawk.Syntax.Term.Source    as Source
import qualified Language.Hawk.Syntax.Pattern.Source as Source
import qualified Language.Hawk.Syntax.Term.Scoped    as Scoped
import qualified Language.Hawk.Syntax.Pattern.Scoped as Scoped


-----------------------------------------------------------------------
-- Scope Check
-----------------------------------------------------------------------

type ScopeCheckT m a = StateT (Set Text) (ReaderT (Set Text) m) a


scopecheck :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
             , MonadChronicle (Bag e) m, AsNcErr e
             ) => Image Term -> m Image STerm 
scopecheck img = do
  let tns1 = Set.map readName $ Set.fromList (img^..imgFns.traversed.fnName)
      tns2 = Set.fromList $ concatMap structNames (img^.imgTStructs) 
      tns = tns1 <> tns2
      tys = Set.fromList $ map structTName (img^.imgTStructs) 
      env = Env.new tns tys
      
  logInfo (_NcStarted # tns)
  condemn $ do
    mapM_ (namecheckFn env) (img^.imgFns)
    mapM_ (namecheckSig env) (img^.imgSigs)
    mapM_ (namecheckStruct env) (img^.imgTStructs)
    mapM_ (namecheckFixity env) (img^.imgFixity)
    mapM_ (namecheckForeign env) (img^.imgForeign)
  logInfo (_NcFinished # ())

  return img


scopecheckTerm
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e
     )
  => Unscoped.Term -> ScopeCheckT m ScopedTerm
scopecheckTerm = \case
    Unscoped.TVar x ->
      return $ Scoped.TVar x

    Unscoped.TLit l ->
      return $ Scoped.TLit l

    _ -> undefined


namecheckFn :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
              , MonadChronicle (Bag e) m, AsNcErr e
              ) => Env -> Fn -> m ()
namecheckFn env (Fn _ xs t) =
  void $ namecheckTerm (env', locTerm t) t
  where env' = Env.insertTerms env (concatMap patNames xs)



namecheckTerm :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
            , MonadChronicle (Bag e) m, AsNcErr e
            ) => (Env, Loc) -> Term -> m Env
namecheckTerm s@(env, l) = \case
  TVar n -> do
    unless (env `Env.checkTerm` n) 
           $ disclose $ One (_UndeclaredNameFound # (n, l))
    return env

  TApp f x -> do
    namecheckTerm s f
    namecheckTerm s x
    return env

  TLam n e -> do
    let env' = Env.insertTerm (readName n) env
    namecheckTerm (env', l) e
    return env

  TLet (n, e1) e2 -> do
    let env' = Env.insertTerm (readName n) env
    namecheckTerm (env', l) e1
    namecheckTerm (env', l) e2
    return env

  TLit _ ->
    return env -- Literals cannot contain names

  TCon n -> do
    unless (env `Env.checkTerm` n) 
           $ disclose $ One (_UndeclaredNameFound # (n, l))
    return env
            
  TPrim _ x1 x2 -> do
    namecheckTerm s x1
    namecheckTerm s x2
    return env -- Primitive instructions cannot contain names


  TIf e1 e2 e3 -> do
    namecheckTerm s e1
    namecheckTerm s e2
    namecheckTerm s e3


  TDup n ->
    namecheckTerm s (varName $ Name n)
    

  TFree ns e ->
    let env' = foldr (Env.deleteTerm . readName) env ns
    in namecheckTerm (env', l) e


  THint t e -> namecheckTerm s t >> namecheckTerm s e
  TLoc l' e -> namecheckTerm (env, l') e
  TParen e  -> namecheckTerm s e




namecheckPat
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => (Env, Loc) -> Pat -> m Env
namecheckPat s@(env, l) = \case
  PVar n ->
    return $ Env.insertTerm n env

  PLit _ -> return env
  PWild  -> return env

  PAs n p -> do
    env' <- namecheckPat s p
    return $ Env.insertTerm n env'
  
  PCon n ps -> do
    env' <- foldM (\env1 p -> namecheckPat (env1,l) p) env ps
    unless (env' `Env.checkTerm` n)
           $ disclose $ One (_UndeclaredNameFound # (n, l))
    return env'

  PParen p -> namecheckPat s p
  PLoc l' p -> namecheckPat (env, l') p
  PHint t p -> namecheckTerm s t >> namecheckPat s p



namecheckSig
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => Env -> Sig -> m ()
namecheckSig env (Sig _ t) = 
  void $ namecheckTerm (env, locTerm t) t


{-
namecheckType
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => (Env, Loc) -> Type -> m Env
namecheckType s@(env, l) = \case
  TVar n ->
    -- No need to check type variables
    return env

  TCon n -> do
    unless (env `Env.checkType` n) 
          $ disclose $ One (_UndeclaredNameFound # (n, l))
    return env
  
  TApp f x -> do
    namecheckType s f
    namecheckType s x
    return env

  TArr a b -> do
    namecheckType s a
    namecheckType s b
    return env

  TLoli a b -> do
    namecheckType s a
    namecheckType s b
    return env

  TKind _ t -> do
    -- Don't check kinds (which don't contain names)
    namecheckType s t
    return env

  TLoc _ t -> do
    namecheckType s t
    return env

  TParen t -> do
    namecheckType s t
    return env

  TForall ns t -> do
    let env' = Env.insertTypes env ns
    namecheckType (env', l) t
    return env
-}


namecheckStruct
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
      , MonadChronicle (Bag e) m, AsNcErr e )
  => Env -> TypeS -> m ()
namecheckStruct env (TypeS _ tvs cs) = 
  mapM_ (namecheckTCon env') cs
  where env' = Env.insertTypes env (unL <$> tvs)


namecheckTCon
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => Env -> TypeCon -> m ()
namecheckTCon env = \case
  TypeCon _ ts -> mapM_ (\t -> namecheckTerm (env, locTerm t) t) ts 
  RecCon _ ls -> mapM_ (namecheckRecLabel env) ls

namecheckRecLabel
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => Env -> RecLabel -> m ()
namecheckRecLabel env (RecLabel _ t) =
  void $ namecheckTerm (env, locTerm t) t



namecheckFixity
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => Env -> Fixity -> m ()
namecheckFixity env (Fixity _ _ ops) =
  forM_ ops $ \(L l n) ->
    unless (Env.checkTerm env n)
           $ disclose $ One (_UndeclaredNameFound # (n, l))

namecheckForeign
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
      , MonadChronicle (Bag e) m, AsNcErr e )
  => Env -> Foreign -> m ()
namecheckForeign env = \case
  ForeignImport _ _ (L l n) t ->
    if Env.checkTerm env n
      then void $ namecheckTerm (env, locTerm t) t
      else disclose $ One (_UndeclaredNameFound # (n, l))
  
  ForeignExport _ (L l n) ->
    unless (Env.checkTerm env n)
           $ disclose $ One (_UndeclaredNameFound # (n, l))