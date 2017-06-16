{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.DepExpansion where

import Control.Lens
import Data.Text (Text)

import Language.Hawk.Syntax

data EDep =
  EDep
    { _edepPath :: [Text]
    , _edepExclude :: Bool
    , _edepQual :: Bool
    , _edepAlias :: Maybe Text
    } deriving (Show)


expandDep :: Dependency -> [EDep]
expandDep (Dep isQ p mayA) = 
    map mkEDep edps
  where
    mkEDep (ns, ex) = EDep ns ex isQ mayA
    edps = expandDepPath ([], False) p

expandDepPath :: ([Text], Bool) -> DepPath -> [([Text], Bool)]
expandDepPath (ns, ex) dp =
    case dp of
      DepPath n dp' ->
          expandDepPath (n:ns, ex) dp'
      
      DepBase n ->
          [(reverse (n:ns), ex)]
      
      DepSpecify ex' dps -> 
          concatMap (expandDepPath (ns, ex')) dps

makeLenses ''EDep