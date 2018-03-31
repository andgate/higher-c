module Data.SnocList where


newtype SnocList a = SnocList { unSnocList :: [a] }
  deriving (Eq, Show)

instance Semigroup (SnocList a) where
  (<>) (SnocList xs) (SnocList ys) = SnocList $ ys ++ xs

instance Monoid (SnocList a) where
  mempty = SnocList []

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList $ x : xs

getSnocList :: SnocList a -> [a]
getSnocList = reverse . unSnocList
