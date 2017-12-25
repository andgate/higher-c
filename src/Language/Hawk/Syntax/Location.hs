{-# LANGUAGE  FlexibleInstances
            , BangPatterns
            , DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Location where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Text (pack)
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as P


-- Location wrapper
data L a = L { unLoc :: Loc
             , unL   :: a
             }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


wrapL :: (a, Loc) -> L a
wrapL (x, l) = L l x

unwrapL :: L a -> (a, Loc)
unwrapL (L l x) = (x, l)


data Loc
  = Loc
    { _locPath  :: !FilePath
    , _locReg   :: {-# UNPACK #-} !Region 
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


data Region
  = R
    { _regStart :: {-# UNPACK #-} !Position
    , _regEnd   :: {-# UNPACK #-} !Position
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


data Position
  = P
    { _posLine    :: {-# UNPACK #-} !Int
    , _posColumn  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Read, Show, Data, Typeable, Generic)


instance Ord Position where
    compare (P l1 c1) (P l2 c2)
      | l1 == l2  = c1 `compare` c2
      | otherwise = l1 `compare` l2


makeClassy ''Loc
makeClassy ''Region
makeClassy ''Position

-- -----------------------------------------------------------------------------
-- Classy Instances  

class Locatable a where
    locOf :: a -> Loc

-- Location can be taken from any foldable functor
instance {-# OVERLAPPABLE #-} (Foldable f, Functor f, Locatable a) => Locatable (f a) where
    locOf = fold . fmap locOf

instance HasRegion Loc where
    region = locReg . region

-- Can't make a HasPosition instance for region, since it has two positions!

-- -----------------------------------------------------------------------------
-- Helpers

mkRegion :: HasPosition a => a -> a -> Region
mkRegion start end = R (start^.position) (end^.position)

stretch :: HasPosition a => a -> Int -> Region
stretch a n = mkRegion p1 p2
  where
    p1@(P l c) = a^.position
    p2 = P l (c + n)

-- -----------------------------------------------------------------------------
-- Helper Instances,

instance Monoid Loc where
    mempty = Loc "" mempty
    mappend (Loc fp r1) (Loc _ r2)
      = Loc fp (r1 <> r2)

instance Monoid Region where
    mempty
      = R (P 0 0) (P 0 0)

    mappend (R s1 e1) (R s2 e2)
      = R (min s1 s2)
          (max e1 e2 )


-- -----------------------------------------------------------------------------
-- Pretty Instances   

instance P.Pretty a => P.Pretty (L a) where
    pretty (L loc a) =
       P.pretty a
       P.<$>
       P.textStrict "located at" P.<+> P.pretty loc


instance P.Pretty Loc where
    pretty loc =
       P.textStrict (pack $ loc^.locPath) P.<> P.textStrict ":" P.<> P.pretty (loc^.locReg)


instance P.Pretty Region where
  pretty (R s e)
    | s == e
      = P.pretty s
    | otherwise
      = P.pretty s P.<> P.textStrict "-" <> P.pretty e


instance P.Pretty Position where
  pretty (P l c) =
    P.pretty (l+1) P.<> P.textStrict ":" <> P.pretty (c+1)


-- -----------------------------------------------------------------------------
-- Binary Instances

instance Binary a => Binary (L a)
instance FromJSON a => FromJSON (L a)
instance ToJSON a => ToJSON (L a)

instance Binary Loc
instance FromJSON Loc
instance ToJSON Loc

instance Binary Region
instance FromJSON Region
instance ToJSON Region

instance Binary Position
instance FromJSON Position
instance ToJSON Position
