{-# LANGUAGE  LambdaCase
            , OverloadedStrings
            , FlexibleContexts
  #-}
module Language.Hawk.CPS.Transform where


import Control.Lens
import Control.Monad
import Control.Monad.Gen.Class
import Data.Text (Text, pack)
import Language.Hawk.CPS.Syntax
import Language.Hawk.NameGen

import qualified Language.Hawk.Syntax as Hk



cps :: (MonadGen Int m)
    => Hk.Exp -> (AExp -> m CExp) -> m CExp
cps e c = \case
  Hk.EVar x -> c $ CVar x

  Hk.EApp a b -> do
    arg <- newName
    cont <- CLam arg <$> c (CVar arg)
    cps a $ \fa ->
      cps b $ \fb ->
      newName >>= \pair ->
      return $ CLet pair (Pair fb cont) (CJump fa (CVar pair))

  Hk.ELam p e -> do
    [pairArg, newCont, newArg] <- replicateM 3 newName
    let e' = e -- using bound, instantiate newArg as a var in e? How to replicate this?
    ce <- cps e' (return . CJump (CVar newCont))
    c (CLam pairArg
       $ CLet newArg  (ProjL pairArg)
       $ CLet newCont (ProjR pairArg)
       $ ce)

  Hk.ELet b e -> undefined

  Hk.ELit l -> c $ CLit (cpsLit l)

  Hk.ECon x -> undefined

  Hk.EPrim i -> undefined

  Hk.EIf p a b -> let c' = \pc -> CIf pc <$> cps a c <*> cps b c
                  in cps p c'
                     
  Hk.EDup n -> undefined

  Hk.EFree ns e -> undefined

  Hk.EType t e -> undefined

  Hk.ELoc l e -> undefined

  Hk.EParen e -> undefined

  
  


cpsLit :: Hk.Lit -> Lit
cpsLit l = undefined
