{-# LANGUAGE  OverloadedStrings
            , FlexibleInstances
            , FlexibleContexts
            , GADTs
            , LambdaCase
            , MultiParamTypeClasses
            , FunctionalDependencies
            , RankNTypes
            , TemplateHaskell
            , GeneralizedNewtypeDeriving
            , TypeSynonymInstances
            , UndecidableInstances
            , StandaloneDeriving
  #-}
module Language.Hawk.TypeCheck where

import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control

import Language.Hawk.TypeCheck.Environment (Env)
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.Message
import Language.Hawk.TypeCheck.State

import Language.Hawk.Syntax

-----------------------------------------------------------------------
-- Type Checking
-----------------------------------------------------------------------


infertype ::
  ( MonadLog (WithSeverity msg) m, AsTcMsg msg
  , MonadChronicle (Bag e) m, AsTcErr e
  )
  => Term -> m (Term, Type)
infertype t = tcTerm (t, Nothing)


checktype ::
  ( MonadLog (WithSeverity msg) m, AsTcMsg msg
  , MonadChronicle (Bag e) m, AsTcErr e
  )
  => Term -> Type -> m (Term, Type)
checktype t expectedTy = do
  nf <- whnf expectedTy
  tcTerm (tm, Just nf)

whnf ::
  ( MonadLog (WithSeverity msg) m, AsTcMsg msg
  , MonadChronicle (Bag e) m, AsTcErr e
  )
  => Type -> m Type
whnf t = Nothing


tcTerm ::
  ( MonadLog (WithSeverity msg) m, AsTcMsg msg
  , MonadChronicle (Bag e) m, AsTcErr e
  )
  => (Term, Maybe Type) -> m (Term, Type)
tcTerm expectedTy = \case
  TVar t -> case expectedTy of
              Just t' -> undefined
              Nothing -> do
                ty <- lookupTy x
                return (t, ty)

  _ -> undefined

inferLit :: Lit -> Type
inferLit = \case
  IntLit _   -> TCon "Int"
  FloatLit _ -> TCon "Float"
  CharLit _  -> TCon "Char"
  BoolLit _  -> TCon "Bool"


inferPrimInstr :: PrimInstr -> Type
inferPrimInstr = \case
  PrimAdd  -> tFun2 tInt tInt tInt
  PrimFAdd -> tFun2 tFloat tFloat tFloat
  PrimSub  -> tFun2 tInt tInt tInt
  PrimFSub -> tFun2 tFloat tFloat tFloat
  PrimMul  -> tFun2 tInt tInt tInt
  PrimFMul -> tFun2 tFloat tFloat tFloat
  PrimDiv  -> tFun2 tInt tInt tInt
  PrimUDiv -> error "Unsigned division not implemented in typechecker."
  PrimSDiv -> error "Short division not implemented in typechecker."
  PrimFDiv -> tFun2 tFloat tFloat tFloat
  
  PrimEq     -> tFun2 tInt tInt tBool
  PrimLt     -> tFun2 tInt tInt tBool
  PrimLtEq   -> tFun2 tInt tInt tBool
  PrimGt     -> tFun2 tInt tInt tBool
  PrimtGtEq  -> tFun2 tInt tInt tBool
  PrimNEq    -> tFun2 tInt tInt tBool
  PrimNLt    -> tFun2 tInt tInt tBool
  PrimNLtEq  -> tFun2 tInt tInt tBool
  PrimNGt    -> tFun2 tInt tInt tBool
  PrimNGtEq  -> tFun2 tInt tInt tBool
  
  PrimBad  -> error "Type checker encountered bad primitive instruction."
  