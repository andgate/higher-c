module Language.Hawk.Compile ( compile
                             , CompilerState (..)
                             , CompilerPhase (..)
                             , Package (..)
                             )
                             where

import Language.Hawk.Compile.Monad
import Language.Hawk.Metadata (collect)
import Language.Hawk.Validate (validate)
--import Language.Hawk.TypeCheck (typecheck)

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
      collect
      validate
      --typecheck
      --runmeta
      --optimize
      --codegen