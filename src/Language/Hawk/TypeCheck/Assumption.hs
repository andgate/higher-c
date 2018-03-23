module Language.Hawk.TypeCheck.Assumption where

import Prelude hiding (lookup)

import Data.Text (Text)

import Language.Hawk.Syntax.Term.Scoped


newtype Assumption = Assumption { assumptions ::[(Text, Term ())] }
  deriving (Eq, Show)


empty :: Assumption
empty = Assumption []


extend :: Assumption -> (Text, Term) -> Assumption
extend (Assumption a) (x, s) = Assumption ((x, s):a)


remove :: Assumption -> Text -> Assumption
remove (Assumption a) var = Assumption $ filter ((/= var) . fst) a


removeMany :: Assumption -> [Text] -> Assumption
removeMany (Assumption a) vars = Assumption $ filter ((`notElem` vars) . fst) a


lookup :: Text -> Assumption -> [Term]
lookup key = map snd . filter ((== key) . fst) . assumptions


lookupMany :: [Text] -> Assumption -> [Term]
lookupMany ks as =
  concatMap (`lookup` as) ks

merge :: Assumption -> Assumption -> Assumption
merge (Assumption a) (Assumption b) = Assumption (a ++ b)


mergeMany :: [Assumption] -> Assumption
mergeMany = foldr merge empty 


singleton :: Text -> Term -> Assumption
singleton key val = Assumption [(key, val)] 


keys :: Assumption -> [Text]
keys (Assumption a) = map fst a
