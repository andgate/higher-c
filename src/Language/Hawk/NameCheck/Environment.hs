module Language.Hawk.NameCheck.Environment where


import Data.Text (Text)
import Data.Set (Set)

import qualified Data.Set as Set


newtype Env = Env { _envFrame :: [Set Text] }


empty :: Env
empty = Env [Set.empty]


check :: Env -> Text -> Bool
check (Env fs) n =
  foldr (\f r -> r || Set.member n f) False fs 

insert :: Text -> Env -> Env
insert n (Env []) = Env [Set.singleton n]
insert n (Env (f:fs)) = Env (f':fs)
  where
    f' = Set.insert n f


insertMany :: Env -> [Text] -> Env
insertMany =
  foldr insert


delete :: Text -> Env -> Env
delete _ (Env []) = Env []
delete n (Env (f:fs))
  | Set.member n f = Env (f':fs)
  | otherwise = Env (f:fs')
  where
    f' = Set.delete n f
    Env fs' = delete n (Env fs)

push :: Env -> Env
push (Env fs) = Env (f:fs)
  where f = Set.empty


pop :: Env -> Env
pop (Env []) = Env []
pop (Env (f:fs)) = Env fs



fromList :: [Text] -> Env
fromList =
  insertMany empty
