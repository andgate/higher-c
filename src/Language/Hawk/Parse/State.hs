{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Parse.State where

import Control.Lens
import Language.Hawk.Compile.PhaseStatus

data ParseState
  = ParseState
    { _psStatus :: PhaseStatus
    }

makeClassy ''ParseState