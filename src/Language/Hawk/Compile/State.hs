{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.State
    ( module Language.Hawk.Compile.State
    ) where

import Control.Lens
import Data.Default.Class
import Data.Map.Lazy (Map)
import Data.Text
import Language.Hawk.Syntax


import qualified Data.Map.Lazy as Map

data HkcState = 
  HkcState
    { _hkcFileTexts :: [(FilePath, String)]
    , _hkcTypes :: Map Text Scheme
    , _hkcDefs :: Map Text [Exp]
    , _hkcDatas :: Map Text DataDecl
    }

makeClassy ''HkcState

instance Default HkcState where
    def =
      HkcState
        { _hkcFileTexts = []
        , _hkcTypes = Map.empty
        , _hkcDefs = Map.empty
        , _hkcDatas = Map.empty
        }
