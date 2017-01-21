{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Language.Hawk.Report.Region where

import Data.Data
import Data.Typeable

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary
import Data.Int
import Text.PrettyPrint.ANSI.Leijen ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Language.Hawk.Parse.Lexer as L


data Region
  = Region
    { start :: Position
    , end   :: Position
    }
    deriving (Eq, Ord, Show, Data, Typeable)
    

data Position
  = Position
    { line    :: {-# UNPACK #-} !Int64
    , column  :: {-# UNPACK #-} !Int64
    }
    deriving (Eq, Ord, Show, Data, Typeable)
    
mkRegion :: HasPosition a => a -> a -> Region
mkRegion start end = Region (toPosition start) (toPosition end)

stretch :: HasPosition a => a -> Int64 -> Region
stretch a n = mkRegion p1 p2
  where
    p1@(Position l c) = toPosition a
    p2 = Position l (c + n)
    
    
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
    toPosition :: a -> Position     

instance HasPosition Position where
    toPosition = id


class HasRegion a where
    toRegion :: a -> Region
    

instance HasRegion Region where
    toRegion = id
  
instance HasPosition a => HasRegion (a, a) where
    toRegion =
      (uncurry mkRegion)


instance HasPosition L.Position where
    toPosition (L.P l c) =
      Position (fromIntegral l) (fromIntegral c)   
      
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
      

instance PP.Pretty Region where
  pretty (Region start end) =
    PP.pretty start
    <> PP.text "-"
    <> PP.pretty end
    
instance PP.Pretty Position where
  pretty (Position line column) =
    PP.text $ show line ++ ":" ++ show column
     
instance Binary Region where
  put r =
    put (start r) >> put (end r)
    
  get = Region <$> get <*> get
  

instance Binary Position where
  put p =
    put (line p) >> put (column p)
    
  get =
    Position <$> get <*> get