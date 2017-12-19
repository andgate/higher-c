module Language.Hawk.TypeCheck.Assumption where

import Prelude hiding (lookup)

import Data.Text (Text)

import Language.Hawk.Syntax.Type


newtype Assumption = Assumption { assumptions ::[(Text, Type)] }
  deriving (Eq, Show)


empty :: Assumption
empty = Assumption []


extend :: Assumption -> (Text, Type) -> Assumption
extend (Assumption a) (x, s) = Assumption ((x, s):a)


remove :: Assumption -> Text -> Assumption
remove (Assumption a) var = Assumption $ filter ((/= var) . fst) a


removeMany :: Assumption -> [Text] -> Assumption
removeMany (Assumption a) vars = Assumption $ filter ((`notElem` vars) . fst) a


lookup :: Text -> Assumption -> [Type]
lookup key = map snd . filter ((== key) . fst) . assumptions


lookupMany :: [Text] -> Assumption -> [Type]
lookupMany ks as =
  concatMap (`lookup` as) ks

merge :: Assumption -> Assumption -> Assumption
merge (Assumption a) (Assumption b) = Assumption (a ++ b)


mergeMany :: [Assumption] -> Assumption
mergeMany = foldr merge empty 


singleton :: Text -> Type -> Assumption
singleton key val = Assumption [(key, val)] 


keys :: Assumption -> [Text]
keys (Assumption a) = map fst a
