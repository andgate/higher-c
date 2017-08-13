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
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax


import qualified Data.Map.Lazy as Map

data HkcState = 
  HkcState
    { _hkcFileTexts :: [(FilePath, Text)]
    , _hkcDefs :: Map Text [Def]
    , _hkcDatas :: Map Text DataDecl
    , _hkcTypes :: Map Var Scheme
    }

makeClassy ''HkcState

instance Default HkcState where
    def =
      HkcState
        { _hkcFileTexts = []
        , _hkcParseState = def
        , _hkcDefs = Map.empty
        , _hkcDatas = Map.empty
        , _hkcTypes = Map.empty
        }


instance HasParseState HkcState where
  parseState = hkcParseState