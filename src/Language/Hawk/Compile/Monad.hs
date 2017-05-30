{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings #-}
module Language.Hawk.Compile.Monad where


import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Except
import Data.Binary
import Data.Data
import Data.Monoid
import Data.Text (Text)
import Data.Typeable
import Database.Persist.TH
import Language.Hawk.Compile.Options

import qualified Data.Text as Text



-------------------------------------------------------------------------------
-- Compiler Monad

-- | Main compiler driver a monad.
type Compiler = StateT CompilerState IO


-- | Run the compiler pipeline.
runCompiler
  :: CompilerState
  -> Compiler ()
  -> IO ()
runCompiler = flip evalStateT


-------------------------------------------------------------------------------
-- Package Information

data Package =
  Package 
    { pkgName     :: Text
    , pkgSrcDir   :: Text
    }

-------------------------------------------------------------------------------
-- Compiler State

data CompilerState = 
  CompilerState
    { cArch     :: Text
    , cOS       :: Text
    , cRoot     :: Text
    , cPkgs     :: [Package]
    , cOpts     :: Opts
    }


defState :: CompilerState
defState = 
  CompilerState
    { cArch = "x86_64"
    , cOS   = "Win10"
    , cRoot = ""
    , cPkgs = []
    , cOpts = defOpts
    }