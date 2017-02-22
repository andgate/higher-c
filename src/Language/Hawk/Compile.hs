module Language.Hawk.Compile ( compile
                             , CompilerState (..)
                             , CompilerPhase (..)
                             )
                             where

import Language.Hawk.Compile.Monad

import qualified Language.Hawk.Metadata           as MD
import qualified Language.Hawk.Parse              as P
import qualified Language.Hawk.Syntax.Item        as I
import qualified Language.Hawk.Syntax.Expression  as E
import qualified Language.Hawk.Syntax.Module      as M
  
  
compile
  :: CompilerState
  -> IO ()
compile s =
  runCompiler s $
    do
      MD.collect
            
      return ()