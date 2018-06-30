{-# LANGUAGE FlexibleContexts #-}
module Language.Hawk.Compile
        ( hkc
        , module Language.Hawk.Compile.Config
        ) where


import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Data.Bag

import Language.Hawk.Dump
import Language.Hawk.Syntax
import Language.Hawk.Load
import Language.Hawk.Lex
import Language.Hawk.Parse
import Language.Hawk.NameCheck
import Language.Hawk.TypeCheck
--import Language.Hawk.KindsCheck
--import Language.Hawk.LinearCheck

--import Language.Hawk.Target.CPS
--import Language.CPS.Target.LLVM

import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad


-----------------------------------------------------------------------
-- Hawk Compiler
-----------------------------------------------------------------------

hkc :: HkcConfig -> IO ()
hkc cfg = runHkc (compile cfg)


compile
  :: ( MonadLog (WithSeverity msg) m, AsHkcMsg msg, AsLdMsg msg, AsLxMsg msg, AsPsMsg msg, AsNcMsg msg, AsTcMsg msg, AsKcMsg msg, AsLcMsg msg
     , MonadChronicle (Bag e) m, AsHkcErr e, AsLdErr e, AsLxErr e, AsPsErr e, AsNcErr e, AsTcErr e, AsKcErr e, AsLcErr e
     , MonadIO m, HasHkcConfig c
     )
  => c -> m ()
compile conf =
  phase1 >>= phase2 >> return ()

  where
    phase1 =
      condemn
      $ loadFiles (conf^.hkcSrcFiles)
          >>= lexMany             >>= dumpLx conf
          >>= parseMany           >>= dumpPs conf

    phase2 img =
      namecheck img     >>= dumpNc conf
      >>= typecheck     >>= dumpTc conf
      -- >>= kindscheck    >>= dumpKc conf
      -- >>= linearcheck   >>= dumpLc conf
