module Language.Hawk.Closure where

import Data.Text       (Text)
import Data.Map.Strict (Map)
import Language.Hawk.Syntax.Bound

import qualified Data.Map.Strict as M


data Closure = Closure (Map Text Term)

-- Put some helpers here
empty :: Closure
empty = Closure $ M.empty