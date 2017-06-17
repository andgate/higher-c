{-# LANGUAGE  FlexibleInstances
            , OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Report.Region where

import Control.Lens
import Data.Binary
import Text.PrettyPrint.ANSI.Leijen ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Region
  = R
    { _regStart :: Position
    , _regEnd   :: Position
    }
    deriving (Eq, Ord, Show)


data Position
  = P
    { _posLine    :: {-# UNPACK #-} !Int
    , _posColumn  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord, Show)


makeLenses ''Position
makeClassy ''Region

-- -----------------------------------------------------------------------------
-- Helpers
    
mkRegion :: HasPosition a => a -> a -> Region
mkRegion start end = R (toPosition start) (toPosition end)

stretch :: HasPosition a => a -> Int -> Region
stretch a n = mkRegion p1 p2
  where
    p1@(P l c) = toPosition a
    p2 = P l (c + n)


-- -----------------------------------------------------------------------------
-- Has Position Class

class HasPosition a where
    toPosition :: a -> Position     

instance HasPosition Position where
    toPosition = id

-- -----------------------------------------------------------------------------
-- Pretty Instances   

instance PP.Pretty Region where
  pretty (R start end) =
    PP.pretty start
    <> PP.text "-"
    <> PP.pretty end


instance PP.Pretty Position where
  pretty (P line column) =
    PP.text $ show line ++ ":" ++ show column


-- -----------------------------------------------------------------------------
-- Binary Instances

instance Binary Region where
  put (R a b) =
    put a >> put b
    
  get = R <$> get <*> get
  

instance Binary Position where
  put (P l c) =
    put l >> put c
    
  get =
    P <$> get <*> get

-- -----------------------------------------------------------------------------
-- Helper Instances

instance Monoid Region where
    mempty = R (P 0 0) (P 0 0)
    mappend (R start _) (R _ end) =
      R start end