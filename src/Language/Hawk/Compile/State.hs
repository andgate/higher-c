{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.State
    ( module Language.Hawk.Compile.State
    , module Language.Hawk.NameCheck.State
    , module Language.Hawk.TypeCheck.State
    ) where

import Control.Lens
import Language.Hawk.NameCheck.State
import Language.Hawk.TypeCheck.State


data HkcState = 
  HkcState
    { _hkcNameCheckState :: NameCheckState
    , _hkcTypeCheckState :: TypeCheckState
    }

makeClassy ''HkcState

instance HasNameCheckState HkcState where
  nameCheckState = hkcNameCheckState

instance HasTypeCheckState HkcState where
  typeCheckState = hkcTypeCheckState