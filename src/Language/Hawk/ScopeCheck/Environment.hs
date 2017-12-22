{-# LANGUAGE LambdaCase #-}
module Language.Hawk.ScopeCheck.Environment where

import Data.Default.Class
import Data.Text (Text)
import Data.Set (Set)

import qualified Data.Set as Set


data Env =
  Env { _envTerms :: [Set Text]
      , _envTypes  :: Set Text
      }


instance Default Env where
  def = empty


empty :: Env
empty = Env [Set.empty] Set.empty


new :: Set Text -> Set Text -> Env
new tns tys =
  Env { _envTerms = [tns]
      , _envTypes = tys
      }


checkTerm :: Env -> Text -> Bool
checkTerm (Env tns _) n =
  foldr (\ns r -> r || Set.member n ns) False tns


checkType :: Env -> Text -> Bool
checkType (Env _ tys) n =
  Set.member n tys


insertTerm :: Text -> Env -> Env
insertTerm n (Env [] tys) = Env [Set.singleton n] tys
insertTerm n (Env (tn:tns) tys) = Env (tn':tns) tys
  where
    tn' = Set.insert n tn


insertTerms :: Env -> [Text] -> Env
insertTerms =
  foldr insertTerm


deleteTerm :: Text -> Env -> Env
deleteTerm _ (Env [] tys) = Env [] tys
deleteTerm n (Env (tn:tns) tys)
  | Set.member n tn = Env (tn':tns) tys
  | otherwise = Env (tn:tns') tys
  where
    tn' = Set.delete n tn
    Env tns' _ = deleteTerm n (Env tns tys)


pushTerm :: Env -> Env
pushTerm (Env tns tys) = Env (tn:tns) tys
  where tn = Set.empty


popTerm :: Env -> Env
popTerm = \case
  Env [] tys -> Env [] tys
  Env (f:fs) tys -> Env fs tys


insertType :: Text -> Env -> Env
insertType n (Env tns tys) = Env tns (Set.insert n tys)

insertTypes :: Env -> [Text] -> Env
insertTypes =
  foldr insertType