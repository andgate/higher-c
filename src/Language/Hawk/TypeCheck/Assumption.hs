module Language.Hawk.TypeCheck.Assumption where

import Language.Hawk.Syntax.Type

import Data.Text (Text)
import Data.Foldable hiding (toList)

newtype Assumption = Assumption { assumptions :: [(Text, Type)]}
  deriving (Eq, Show)


empty :: Assumption
empty = Assumption []

extend :: Assumption -> (Text, Type) -> Assumption
extend (Assumption as) a = Assumption (a:as)

remove :: Assumption -> Text -> Assumption
remove (Assumption a) v = Assumption $ filter (\(n,_ ) -> n /= v) a

lookup :: Text -> Assumption -> [Type]
lookup k (Assumption a) = map snd . filter (\(n, _) -> n == k) $ a

merge :: Assumption -> Assumption -> Assumption
merge (Assumption a) (Assumption b) = Assumption (a ++ b)

mergeAssumptions :: [Assumption] -> Assumption
mergeAssumptions = foldl' merge empty

singleton :: Text -> Type -> Assumption
singleton n t = Assumption [(n,t)]

keys :: Assumption -> [Text]
keys (Assumption a) = map fst a