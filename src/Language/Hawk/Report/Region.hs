{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Region where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary
import qualified Text.Parsec.Pos as Parsec


data Region
  = Region
    { start :: Position
    , end   :: Position
    }
    deriving (Eq, Show)
    

data Position
  = Position
    { line    :: Int
    , column  :: Int
    }
    deriving (Eq, Show)
    
    
fromSourcePos :: Parsec.SourcePos -> Position
fromSourcePos sourcePos =
  Position
    (Parsec.sourceLine sourcePos)
    (Parsec.sourceColumn sourcePos)
    
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