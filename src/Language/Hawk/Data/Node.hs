module Language.Hawk.Data.Node
        ( module Language.Hawk.Data.Node,
          module Language.Hawk.Data.Span
        ) where

import Language.Hawk.Data.Span

import Data.Generics
import Data.Monoid

-- | Parsed entity attribute
data NodeInfo = NodeInfo
                { node_file   :: String
                , node_span   :: Span
                }
           deriving (Show,Eq,Ord)

{-
instance Show NodeInfo where
    show (NodeInfo n s) = n ++ (show s)
-}

instance HkSpan NodeInfo where
  spanOf (NodeInfo _ s) = s


instance Monoid NodeInfo where
  mempty = NodeInfo "<unknown file>" mempty
  
  mappend (NodeInfo n1 s1) (NodeInfo n2 s2)
    | n1 == n2 = NodeInfo n1 (s1 <> s2)
    | otherwise = error "Cannot combine nodes from different files"

-- | a class for convenient access to the attributes of an attributed object
class HkNode a where
  nodeInfo :: a -> NodeInfo
  

instance HkNode NodeInfo where
  nodeInfo = id
instance (HkNode a, HkNode b) => HkNode (Either a b) where
  nodeInfo = either nodeInfo nodeInfo

nodesInfo :: HkNode n => [n] -> NodeInfo  
nodesInfo []      = mempty
nodesInfo (x:[])  = nodeInfo x
nodesInfo xs      = nodeInfo (head xs) <> nodeInfo (last xs)