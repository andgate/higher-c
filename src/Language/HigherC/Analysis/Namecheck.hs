{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Language.HigherC.Analysis.Namecheck where

import Data.Foldable
import Data.List
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding (group)
import Data.Text (Text, pack)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Language.HigherC.Syntax.Concrete
import Language.HigherC.Syntax.Location

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty  as NE


data NameError
  = NameNotFound Loc Text
  | NameConflict Text Loc Loc

instance Pretty NameError where
    pretty = \case
        NameNotFound l n ->
            pretty l <+> "Name Not Found:" <+> pretty n
        
        NameConflict n l l' ->
            vsep [ "Name conflict detected between"
                 , pretty n <+> "at" <+> pretty l'
                 , "and" <+> pretty n <+> "at" <+> pretty l
                 ]
{-
namecheckTerms :: Map Text Loc -> [Term] -> [NameError]
namecheckTerms d = mconcat . map (namecheck d)

namecheck :: Map Text Loc -> Term -> [NameError]
namecheck vs = \case
  TVar (v, l) -> 
    if v `Map.member` vs
      then []
      else [NameNotFound l v]

  TVal _ -> []
  
  TApp f xs -> 
    let nc = namecheck vs
        f' = nc f
        xs' = nc <$> NE.toList xs
    in f' ++ fold xs'

  TPrim _ x y ->
    let x' = namecheck vs x
        y' = namecheck vs y
    in x' ++ y'
  
  TLam ns body ->
    let ns' = checkConflicts $ NE.toList ns
        vs' = vs `Map.union` (Map.fromList $ NE.toList ns)
        body' = namecheck vs' body
    in ns' ++ body'

  TLet bs body ->
    let bs' = NE.toList bs
        ns = fst <$> bs'
        xs = snd <$> bs'
        vs' = foldr (\(n, l) vs -> Map.insert n l vs) vs ns
        xs' = namecheck vs' <$> xs
        body' = namecheck vs' body
    in fold xs' ++ body'
  
  TLoc    _ t  -> namecheck vs t -- you'd think this would be useful here but nah
  TParens t    -> namecheck vs t
  TWild        -> []


checkConflicts :: [(Text, Loc)] -> [NameError]
checkConflicts =
    map (\(n, l, l') -> NameConflict n l l') . conflicts

conflicts :: Ord a => [(a, Loc)] -> [(a, Loc, Loc)]
conflicts = snd . dups' Map.empty []
  where
    dups' seen ns' ns = foldr go (seen, ns') ns
    go (n, l) (seen, ns') =
      case Map.lookup n seen of
        Nothing -> (Map.insert n l seen, ns')
        Just l' -> (seen, (n, l, l'):ns')

-}
