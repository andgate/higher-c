module Language.Hawk.Report.Result where

import Control.Monad.Except (Except, runExcept)
import Language.Hawk.Report.Error
import Language.Hawk.Report.Info
import Language.Hawk.Report.Warning

import qualified Language.Hawk.Report.Region as R


-- | Result types

data Result r
  = Result 
   { _warnings :: [Warning]
   , _info :: [Info] 
   , _answer :: Either [Error] r
   }