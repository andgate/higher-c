module Language.Hawk.Parse.Layout where

import Control.Monad.Trans.State.Strict (State)
import Lens.Micro.Mtl ((.=), (+=))
import Pipes

import qualified Control.Monad.Trans.State.Strict as State
import qualified Language.Hawk.Parse.Lexer as L

data Layout = 
  Layout [Int]
  deriving (Eq, Ord, Show)

layout :: Pipe L.Token L.Token (State Layout) ()
layout = return ()