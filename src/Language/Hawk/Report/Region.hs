{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Language.Hawk.Report.Region where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary
import Data.Int
import qualified Text.Trifecta.Delta as Trifecta


data Region
  = Region
    { start :: Position
    , end   :: Position
    }
    deriving (Eq, Show)
    

data Position
  = Position
    { line    :: {-# UNPACK #-} !Int64
    , column  :: {-# UNPACK #-} !Int64
    }
    deriving (Eq, Show)
    
mkRegion :: HasPosition a => a -> a -> Region
mkRegion start end = Region (getPosition start) (getPosition end)
    
merge :: Region -> Region -> Region
merge (Region start _) (Region _ end) =
  Region start end
  
  
toString :: Region -> String
toString (Region start end) =
  case line start == line end of
    False ->
      "between lines " ++ show (line start)
      ++ " and " ++ show (line end)
      
    True ->
      "on line " ++ show (line end) ++ ", column "
      ++ show (column start) ++ " to " ++ show (column end)


class HasPosition a where
    getPosition :: a -> Position     

instance HasPosition Position where
    getPosition = id
    


class HasRegion a where
    getRegion :: a -> Region
    

instance HasRegion Region where
    getRegion = id
  
instance HasPosition a => HasRegion (a, a) where
    getRegion =
      (uncurry mkRegion)
      
      
instance HasPosition Trifecta.Delta where
  getPosition (Trifecta.Columns _ _) =
    error "Delta Columns constructor does not have position"
    
  getPosition (Trifecta.Tab _ _ _) =
    error "Delta Tab constuctor does not have position"
    
  getPosition (Trifecta.Lines line column _ _) =
    Position line column
    
  getPosition (Trifecta.Directed _ line column _ _) =
    Position line column
      
      
instance Json.ToJSON Region where
  toJSON (Region start end) =
    Json.object
      [ "start" .= start
      , "end"   .= end
      ]
      
      
instance Json.ToJSON Position where
  toJSON (Position line column) =
    Json.object
      [ "line"    .= line
      , "column"  .= column
      ]
      
      
instance Binary Region where
  put r =
    put (start r) >> put (end r)
    
  get = Region <$> get <*> get
  

instance Binary Position where
  put p =
    put (line p) >> put (column p)
    
  get =
    Position <$> get <*> get