module Language.Hawk.Report.Result where

import Control.Monad.Except (Except, runExcept)


import qualified Language.Hawk.Data.Bag as Bag
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


-- | Result types

type Result warning error result
  = Either [error] ([warning], result)