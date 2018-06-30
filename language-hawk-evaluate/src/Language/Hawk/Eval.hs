{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
 #-}
module Language.Hawk.Eval where


import Control.Monad.Except
import Control.Monad.Reader
import Data.Either (fromRight)
import Language.Hawk.Syntax.Abstract


newtype Env a = Env { unEnv :: ReaderT [Value] (Except String) a }
  deriving (Functor, Applicative, Monad, MonadReader [Value], MonadError String)

runEnv :: [Value] -> Env a -> Either String a
runEnv vs env = runExcept (runReaderT (unEnv env) vs) 

evalChk :: TermChk -> Env Value
evalChk = \case
  TAnn e _ -> evalInf e
  TFree x -> return $ vfree x
  TBound i -> reader (!! i)
  TApp e e' -> vapp <$> (evalChk e) <*> (evalInf e')

evalInf :: TermInf -> Env Value
evalInf = \case
  TInf i -> evalChk i
  TLam e -> do
    vs <- ask
    let f v = case runEnv (v:vs) (evalInf e) of
                Left e -> error e
                Right v' -> v' 
    return $ VLam f