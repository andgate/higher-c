{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
 #-}
module Language.Hawk.Eval where


import Control.Monad.Reader
import Data.Text (Text, pack)
import Language.Hawk.Closure (Closure)
import Language.Hawk.Syntax.Bound
import Language.Hawk.Syntax.Prim
import Language.Hawk.Value
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import qualified Language.Hawk.Closure as CL


eval :: Closure -> Term -> Term
eval clos t = runEval clos (evalTerm t)

newtype Eval a = Eval { unEval :: ReaderT Closure FreshM a } 
  deriving (Functor, Applicative, Monad, MonadReader Closure, Fresh)


runEval :: Closure -> Eval a -> a
runEval clos (Eval m) = runFreshM $ runReaderT m clos

evalTerm :: Term -> Eval Term
evalTerm = \case
  TUni u   -> return $ TUni u
  TVar v -> return $ TVar v -- substitute in from the closure?

  TCon n -> return $ TCon n
  TVal v -> return $ TVal v
  
  TPrim i t1 t2 -> do
    t1' <- evalTerm t1
    t2' <- evalTerm t2
    case (t1', t2') of
      (TVal v1, TVal v2) ->
           return . TVal $ evalInstr (i, v1, v2)
      _ -> return $ TPrim i t1' t2'

  TApp f as -> do
    f' <- evalTerm f
    case f' of
      TLam bnd -> do
        (tele, body)   <- unbind bnd
        (body', tele') <- applyTele body tele as 
        case tele' of
          ScopeNil -> return $ body'
          _        -> return $ TLam (bind tele' body') 
     
      TApp g bs -> evalTerm $ TApp g (bs ++ as)
      _         -> TApp f' <$> mapM evalTerm as


  TLam bnd -> do
    (tele, body) <- unbind bnd
    body' <- evalTerm body
    return $ TLam (bind tele body')

  TPi _ ->
    return $ TUni Type
  
  TSigma _ ->
    return $ TUni Type

  TLet bnd -> do
    (r, body) <- unbind bnd
    let vars = unrec r
        -- Substitute all the terms into the body
        body' = foldr (\(v, Embed rhs) body -> subst v rhs body) body vars
        fvs = toListOf fv body'
    if any (\(v,_) -> v `elem` fvs) vars
      then evalTerm . TLet $ bind (rec vars) body'
      else evalTerm body'


  TAnn tm ty -> evalTerm tm -- ignore annotations!
  TLoc l t   -> evalTerm t


applyTele :: Term -> Tele -> [Term] -> Eval (Term, Tele)
applyTele t (ScopeCons rb) (a:as) = do
  let ((v, _), tele) = unrebind rb
  a' <- evalTerm a
  applyTele (subst v a' t) tele as

applyTele t ScopeNil (_:_) =  error "Too many arguments applied through telescope"
applyTele t tele _ = do
  t' <- evalTerm t
  return (t', tele)