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
import Data.Map.Strict (Map)
import Data.Set
import Data.Text (Text)
import Language.Hawk.Rename.Error
import Language.Hawk.Syntax.Prim


import qualified Data.Map.Strict as Map
import qualified Language.Hawk.Syntax.Source as S

import Language.Hawk.Syntax.Suspension



data RenameEnv a where
  RenameEnvNil :: RenameEnv Text
  RenameEnvCons :: Text -> RenameEnv a -> RenameEnv (Var a)


type Globals = Set Text

newtype Renamer a = Rn { unRn :: ReaderT Globals (Except RnError) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Globals
           , MonadError RnError
           )


runRename :: Globals -> Renamer a -> Either RnError a  
runRename gs rn = runExcept (runReaderT (unRn rn) gs)


rename :: Globals -> S.Term -> Either RnError (Term Text)
rename gs = runRename gs . renameTerm RenameEnvNil


renameEnvLookup :: RenameEnv a -> Text -> a
renameEnvLookup env0 txt = case env0 of
  RenameEnvNil -> txt
  RenameEnvCons txt' env -> if txt == txt'
    then B
    else F (renameEnvLookup env txt)


renameTerm :: RenameEnv a -> S.Term -> Renamer (Term a)
renameTerm env t = Syntax <$> renameSyntax env t

renameSyntax :: RenameEnv a -> S.Term -> Renamer (Syntax a)
renameSyntax env = \case
  S.Type      -> return Type
  S.Linear    -> return Linear
  S.TVar n    -> TVar <$> renameVar env n
  S.TCon n    -> return $ TCon n
  S.TVal v    -> return $ TVal v
  S.TPrim i t t' -> TPrim i <$> renameTerm env t <*> renameTerm env t'
  
  S.TApp t t' ->
      TApp <$> renameTerm env t <*> renameTerm env t'
  
  S.TLam n Nothing body ->
      TLam n Nothing <$> (renameTerm (RenameEnvCons n env) body)
  
  S.TLam n (Just ty) body ->
      TLam n <$> (Just <$> renameTerm env ty) <*> (renameTerm (RenameEnvCons n env) body)

  
  S.TPi n ty body ->
      TPi n <$> (renameTerm env ty) <*> (renameTerm (RenameEnvCons n env) body)
  
  S.TSigma ty ty' ->
      TSigma <$> (renameTerm env ty) <*> (renameTerm env ty')
  
  S.TAnn tm ty -> TAnn   <$> renameTerm env tm <*> renameTerm env ty
  S.TParen t   -> TParen <$> renameTerm env t
  S.TLoc l t   -> TLoc l <$> renameTerm env t


renameVar :: RenameEnv a -> Text -> Renamer a
renameVar env = return . renameEnvLookup env