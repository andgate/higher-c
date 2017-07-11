{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Parse.State where

import Control.Lens
import Data.IntMap.Lazy (IntMap)
import Data.Default.Class

import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax

import qualified Data.IntMap.Lazy as IMap

data ParseState
  = ParseState
    { _psToks :: [[Token]]
    , _psOps  :: IntMap [Operator]
    }


instance Default ParseState where
  def = ParseState 
        { _psToks = []
        , _psOps = IMap.fromList [(n,[]) | n <- [0..9]]
        }

makeClassy ''ParseState