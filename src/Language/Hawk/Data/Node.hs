{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Data.Node
        ( module Language.Hawk.Data.Node,
          module Language.Hawk.Data.Position
        ) where

import Language.Hawk.Data.Position
import Data.Generics

-- | Parsed entity attribute
data NodeInfo = NodePoint 
                { node_pos :: Position
                , node_name :: String
                }
              | NodeSpan
                { node_start  :: Position
                , node_end    :: Position
                , node_name   :: String
                }
           deriving (Data,Typeable,Eq,Ord)

instance Show NodeInfo where
    showsPrec d (NodePoint p n) =
      (showString "(NodeSingle ") . (showsPrec d p) . (showString " ") . (showsPrec d n) . (showString ")")
    showsPrec d (NodeSpan a b n) =
      (showString "(NodeSpan ") . (showsPrec d a) . (showString " ") . (showsPrec d b) . (showString " ") . (showsPrec d n) . (showString ")")


instance Pos NodeInfo where
  posOf (NodePoint pos _) = pos
  posOf (NodeSpan pos _ _) = pos


-- | a class for convenient access to the attributes of an attributed object
class HkNode a where
  nodeInfo :: a -> NodeInfo
instance HkNode NodeInfo where
  nodeInfo = id
instance (HkNode a, HkNode b) => HkNode (Either a b) where
  nodeInfo = either nodeInfo nodeInfo