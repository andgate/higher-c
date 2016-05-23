module Language.Hawk.Report.Annotation where

import Prelude hiding (map)
import Data.Binary
import qualified Language.Hawk.Report.Region as R


data Annotated annot a
  = A annot a
  deriving (Show)
  
type Located a =
  Annotated R.Region a
  
type Commented a =
  Annotated (R.Region, Maybe String) a
  
  
at :: R.Position -> R.Position -> a -> Located a
at start end value =
  A (R.Region start end) value
  
  
merge :: Located a -> Located b -> value -> Located value
merge (A region1 _) (A region2 _) value =
  A (R.merge region1 region2) value
  
  
sameAs :: Annotated info a -> b -> Annotated info b
sameAs (A annot _)  value =
  A annot value
  
  
map :: (a -> b) -> Annotated info a -> Annotated info b
map f (A annot value) =
  A annot (f value)
  
drop :: Annotated info a -> a
drop (A _ value) =
  value
  
  
  
instance (Binary annot, Binary a) => Binary (Annotated annot a) where
  put (A annot a) =
    put annot >> put a
    
    
  get =
    A <$> get <*> get