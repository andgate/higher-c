{-# LANGUAGE  FlexibleInstances
            , BangPatterns
            , DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Syntax.Location where

import Control.Lens
import Data.Binary
import Data.Text (pack)
import GHC.Generics (Generic)
import Text.PrettyPrint.Leijen.Text ((<>))

import qualified Text.PrettyPrint.Leijen.Text as P


data Location
  = Loc
    { _locPath  :: !FilePath
    , _locReg   :: {-# UNPACK #-} !Region 
    }
    deriving (Eq, Ord, Show, Generic)


data Region
  = R
    { _regStart :: {-# UNPACK #-} !Position
    , _regEnd   :: {-# UNPACK #-} !Position
    }
    deriving (Eq, Ord, Show, Generic)


data Position
  = P
    { _posLine    :: {-# UNPACK #-} !Int
    , _posColumn  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord, Show, Generic)

makeClassy ''Location
makeClassy ''Region
makeClassy ''Position

-- -----------------------------------------------------------------------------
-- Classy Instances  

instance HasRegion Location where
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

instance Monoid Region where
    mempty = R (P 0 0) (P 0 0)
    mappend (R start _) (R _ end) =
      R start end

-- -----------------------------------------------------------------------------
-- Pretty Instances   

instance P.Pretty Location where
    pretty loc =
       P.textStrict (pack $ loc^.locPath) <> P.textStrict ":" <> P.pretty (loc^.locReg)

instance P.Pretty Region where
  pretty (R s e)
    | s == e
      = P.pretty s
    | otherwise
      = P.pretty s <> P.textStrict "-" <> P.pretty e


instance P.Pretty Position where
  pretty (P l c) =
    P.pretty l <> P.textStrict ":" <> P.pretty c


-- -----------------------------------------------------------------------------
-- Binary Instances

instance Binary Location
instance Binary Region
instance Binary Position