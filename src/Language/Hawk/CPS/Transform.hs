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
    => Hk.Exp -> (Value -> m Term) -> m Term
cps e c = case e of
  Hk.EVar x -> c $ Var x

  Hk.EApp a b -> do
    arg <- newName
    cont <- Lam arg <$> c (Var arg)
    cps a $ \fa ->
      cps b $ \fb -> do
        pair <- newName
        return $ Let pair (Pair fb cont) (Jump fa (Var pair))

  Hk.ELam p e -> do
    [pairArg, newCont, newArg] <- replicateM 3 newName
    let e' = e -- using bound, instantiate newArg as a var in e? How to replicate this?
    ce <- cps e' (return . Jump (Var newCont))
    c (Lam pairArg
       $ Let newArg  (ProjL pairArg)
       $ Let newCont (ProjR pairArg)
       $ ce)

  Hk.ELet b e -> undefined

  Hk.ELit l -> c $ Lit (cpsLit l)

  Hk.ECon x -> undefined

  Hk.EPrim i a b -> undefined

  Hk.EIf p a b -> let c' = \pc -> If pc <$> cps a c <*> cps b c
                  in cps p c'
                     
  Hk.EDup n -> undefined

  Hk.EFree ns e -> undefined

  Hk.EType t e -> cps e c

  Hk.ELoc l e -> cps e c

  Hk.EParen e -> cps e c

  
  


cpsLit :: Hk.Lit -> Lit
cpsLit l = undefined
