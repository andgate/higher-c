{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.State
    ( module Language.Hawk.Compile.State
    ) where

import Control.Lens
import Data.Default.Class
import Data.Text
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax


data HkcState = 
  HkcState
    { _hkcFileTexts :: [(FilePath, Text)]
    , _hkcFileTokens :: [(FilePath, [Token])]
    , _hkcRootMod :: SrcMod
    }

makeClassy ''HkcState

instance Default HkcState where
    def =
        HkcState
        { _hkcFileTexts = []
        , _hkcFileTokens = []
        , _hkcRootMod = def
        }

instance HasSrcMod HkcState where
    srcMod = hkcRootMod