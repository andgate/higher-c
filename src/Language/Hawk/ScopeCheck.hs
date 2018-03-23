{-# LANGUAGE  LambdaCase
            , FlexibleContexts
  #-}
module Language.Hawk.ScopeCheck where

import Bound
import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control

import Data.Bag
import Data.Bifunctor
import Data.Bitraversable
import Data.Default.Class
import Data.Foldable as Foldable
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)

import Language.Hawk.ScopeCheck.Environment (Env)
import Language.Hawk.ScopeCheck.Error
import Language.Hawk.ScopeCheck.Message
import Language.Hawk.ScopeCheck.State
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Pattern
import Language.Hawk.Syntax.Pattern.Source

import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Language.Hawk.Syntax.Datatype.Scoped     as Scoped
import qualified Language.Hawk.Syntax.Datatype.Source     as Source
import qualified Language.Hawk.Syntax.Definition.Scoped   as Scoped
import qualified Language.Hawk.Syntax.Definition.Source   as Source
import qualified Language.Hawk.Syntax.Term.Scoped         as Scoped
import qualified Language.Hawk.Syntax.Term.Source         as Source


-----------------------------------------------------------------------
-- Types for Scope Checker

type ScEnv = ( Loc            -- Last location above current position
             , HashSet Text   -- Set of valid names
             )

type SourceLib = Source.Lib Source.Term Text
type ScopedLib = Source.Lib Scoped.Term Text

type SourceDef = Source.Def SourceTerm
type ScopedDef = Source.Def ScopedTerm 

type SourceTerm = Source.Term Text
type SourceType = SourceTerm
type SourcePat = Pat SourceTerm Text

type ScopedTerm       = Scoped.Term Text
type ScopedType       = ScopedTerm
type PatScopedTerm    = PatScope Scoped.Term Text
type ScopedPat        = Pat ScopedTerm Text


type SourceDatatype = Source.Datatype Source.Term Text
type ScopedDatatype = Scoped.Datatype Scoped.Term Text

type SourceForeign = Foreign SourceTerm
type ScopedForeign = Foreign ScopedTerm


-----------------------------------------------------------------------
-- Scope Checking

scopecheck
  :: ( MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e
     )
  => SourceLib -> m ScopedLib
scopecheck img = do
  {-
  let tns1 = Set.map readName $ Set.fromList (img^..imgFns.traversed.fnName)
      tns2 = Set.fromList $ concatMap structNames (img^.imgTStructs) 
      tns = tns1 <> tns2
      tys = Set.fromList $ Prelude.map structTName (img^.imgTStructs) 
      env = Env.new tns tys
  -}
      
  logInfo (_ScStarted # mempty)
{-condemn $ runReaderT (mempty, HSet.empty) do
    mapM_ (namecheckFn env) (img^.imgFns)
    mapM_ (namecheckSig env) (img^.imgSigs)
    mapM_ (namecheckStruct env) (img^.imgTStructs)
    mapM_ (namecheckFixity env) (img^.imgFixity)
    mapM_ (namecheckForeign env) (img^.imgForeign)
-}
  logInfo (_ScFinished # ())

  return undefined

-----------------------------------------------------------------------
-- Scope Checking on Definitions

scopecheckStruct
  :: ( MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e 
     )
  => SourceDatatype -> m ScopedDatatype
scopecheckStruct Source.Datatype{} = 
  undefined
{-
  mapM_ (namecheckTCon env') cs
  where env' = Env.insertTypes env (unL <$> tvs)
-}

scopecheckFixity
  :: ( MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e )
  => Fixity -> m Fixity
scopecheckFixity Fixity {} =
  undefined
{-
  forM_ ops $ \(L l n) ->
    unless (Env.checkTerm env n)
            $ disclose $ One (_UndeclaredNameFound # (n, l))
-}


scopecheckForeign
  :: ( MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e )
  => SourceForeign -> m ScopedForeign
scopecheckForeign = \case
  ForeignImport {} -> undefined
  ForeignExport {} -> undefined


-----------------------------------------------------------------------
-- Scope Checking Terms and Patterns

scopecheckTerm 
  :: ( MonadReader (Loc, HashSet Text) m
     , MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e
     )
  => SourceTerm -> m ScopedTerm
scopecheckTerm = \case
  Source.TVar x ->
    return $ Scoped.TVar x

  Source.TLit l ->
    return $ Scoped.TLit l

  Source.TCon n ->
    return $ Scoped.TCon n
            
  Source.TPrim i x1 x2 ->
    Scoped.TPrim i <$> scopecheckTerm x1 <*> scopecheckTerm x2

  Source.TApp f x ->
    Scoped.TApp <$> scopecheckTerm f <*> scopecheckTerm x

  Source.TLam _ _ -> undefined

  Source.TLet ds e -> do
    ds' <- traverse scopecheckDef ds
    e' <- scopecheckTerm e
    return $ error "let scoping is unimplemented"


  Source.TCase e brs ->
    Scoped.TCase
      <$> scopecheckTerm e 
      <*> mapM (uncurry scopecheckBranch) brs


  Source.TDup n ->
    return $ Scoped.TDup (Scoped.TVar n)

  Source.TFree ns e ->
    Scoped.TFree (Scoped.TVar <$> ns) <$> scopecheckTerm e


  Source.TAnnot t e -> 
    Scoped.TAnnot <$> scopecheckTerm e <*> scopecheckTerm t
  
  Source.TSub st t ->
    Scoped.TSub st <$> scopecheckTerm t
  
  Source.TLoc l' t ->
    Scoped.TLoc l' <$> local (bimap (const l') id) (scopecheckTerm t)

  Source.TParen e ->
    Scoped.TParen <$> scopecheckTerm e

  Source.TWild -> 
    return Scoped.TWild



scopecheckDef 
  :: ( MonadReader (Loc, HashSet Text) m
     , MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e
     )
  => Source.Def SourceTerm -> m (Text, Scoped.PatDef (Scoped.Clause void Scoped.Term Text), Maybe ScopedType)
scopecheckDef (Source.Def n cs t) = undefined


scopecheckClause
  :: ( MonadReader (Loc, HashSet Text) m
     , MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e
     )
  => Source.Clause SourceTerm -> m (Scoped.Clause void Scoped.Term Text) 
scopecheckClause (Source.Clause _ _) = undefined

scopecheckBranch
  :: ( MonadReader (Loc, HashSet Text) m
     , MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e
     )
  => SourcePat -> SourceTerm -> m (Pat (PatScope Scoped.Term Text) (), PatScope Scoped.Term Text)
scopecheckBranch p t = do
  p' <- scopecheckPat p
  let vs = Foldable.toList p'
      p'' = void $ abstractPatternTypes vs p'
  t' <- abstract (patternAbstraction vs) <$> scopecheckTerm t
  return (p'', t')


scopecheckPat
  :: ( MonadReader (Loc, HashSet Text) m
     , MonadLog (WithSeverity msg) m, AsScMsg msg
     , MonadChronicle (Bag e) m, AsScErr e )
  => SourcePat -> m (Pat ScopedTerm Text)
scopecheckPat = \case
  PVar h b -> return $ PVar h b

  PWild -> return PWild
  PLit l -> return $ PLit l

  PCon n ps -> undefined

  PAnnot p t ->
    PAnnot <$> scopecheckPat p <*> scopecheckTerm t

  PView t p ->
    PView <$> scopecheckTerm t <*> scopecheckPat p

  PLoc l' p ->
    PLoc l' <$> local (_1 .~ l') (scopecheckPat p)