{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Data.Node
        ( module Language.Hawk.Data.Node,
          module Language.Hawk.Data.Span
        ) where

import Language.Hawk.Data.Span

import Data.List
import Data.Generics
import Data.Monoid

-- | Parsed entity attribute
data NodeInfo = NodeInfo
                { node_file   :: String
                , node_span   :: Span
                }
              | EmptyNode
              deriving (Eq,Ord, Data, Typeable)

{-
instance Show NodeInfo where
    show (NodeInfo n s) = n ++ (show s)
-}

showNode :: (HkNode n) => n -> String
showNode n = f ++ ":" ++ (show s)
  where (NodeInfo f s) = nodeInfo n

instance Show NodeInfo where
    show (NodeInfo n s) = ""
    show (EmptyNode) = ""

instance HkSpan NodeInfo where
  spanOf (NodeInfo _ s) = s
  spanOf (EmptyNode) = mempty


instance Monoid NodeInfo where
  mempty = EmptyNode
  
  mappend EmptyNode EmptyNode = EmptyNode
  mappend EmptyNode n@(NodeInfo _ _) = n
  mappend n@(NodeInfo _ _) EmptyNode = n
  
  mappend n1@(NodeInfo f1 s1) n2@(NodeInfo f2 s2)
    | f1 == f2 = NodeInfo f1 (s1 <> s2)
    | otherwise = error ("Cannot combine nodes from different files.\n" ++ showNode n1 ++ "\n" ++ showNode n2)

-- | a class for convenient access to the attributes of an attributed object
class HkNode a where
  nodeInfo :: a -> NodeInfo
  

instance HkNode NodeInfo where
  nodeInfo = id
instance (HkNode a, HkNode b) => HkNode (Either a b) where
  nodeInfo = either nodeInfo nodeInfo

{-
instance (HkNode a) => HkNode (Maybe a) where
  nodeInfo (Just a) = nodeInfo a
  nodeInfo Nothing = mempty
-}

nodesInfo :: HkNode n => [n] -> NodeInfo  
nodesInfo []      = mempty
nodesInfo (x:[])  = nodeInfo x
nodesInfo xs      = nodeInfo (head xs) <> nodeInfo (last xs)

mergeNodes :: [NodeInfo] -> NodeInfo
mergeNodes = foldl' (<>) mempty