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

import qualified Language.Hawk.Syntax as Hk



newName :: MonadGen Int m
        => m Text
newName = do
  n <- gen
  return $ pack (letters !! n)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']
  

cps :: (MonadGen Int m)
    => (AExp -> m CExp) -> Hk.Exp -> m CExp
cps c = \case
  Hk.EVar x -> c $ CVar x

  Hk.EApp a b -> undefined

  Hk.ELam p e -> do
    [pairArg, newCont, newArg] <- replicateM 3 newName
    let e' = e -- using bound, instantiate newArg as a var in e? How to replicate this?
    ce <- cps (return . CJump (CVar newCont)) e'
    c (CLam pairArg
       $ CLet newArg  (ProjL pairArg)
       $ CLet newCont (ProjR pairArg)
       $ ce)

  Hk.ELet b e -> undefined

  Hk.ELit l -> c $ CLit (cpsLit l)

  Hk.ECon x -> undefined

  Hk.EPrim i -> undefined

  Hk.EIf p a b -> let c' pc = CIf pc <$> cps c a <*> cps c b
                  in cps c' p
                     
  Hk.EDup n -> undefined

  Hk.EFree ns e -> undefined

  Hk.EType t e -> undefined

  Hk.ELoc l e -> undefined

  Hk.EParen e -> undefined

  
  


cpsLit :: Hk.Lit -> Lit
cpsLit l = undefined
