{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Compile.PhaseStatus where

import Control.Lens

data PhaseStatus
  = PhaseSuccess
  | PhaseFailure
  | PhaseInProgress
  | PhaseNotStarted
  deriving (Eq)

makeClassyPrisms ''PhaseStatus