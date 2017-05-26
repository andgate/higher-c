module Language.Hawk.Compile.Options where

import Language.Hawk.Compile.Flags

data Command
    = Build String
    | Clean

data Opts = Opts
    { optCommand :: !Command

    -- Warning Flags
    , optInfoVerbosity :: !Int
    , optWarnVerbosity :: !Int
    , optFlags :: [String]
    }


defOpts :: Opts
defOpts =
  Opts
  { optCommand = Build ""

  -- Warning Flags
  , optInfoVerbosity = 2
  , optWarnVerbosity = 1
  , optFlags = []
  }


class HasVerbosity a where
    verbosityThreshold :: a -> Opts -> Int
    verbosity :: a -> Int


messageFilter :: (HasVerbosity a, HasFlags a) => Opts -> [a] -> [a]
messageFilter o msgs = 
  let
    p msg | (flagOn msg) `elem` (optFlags o)  -- Flagged messages take priority
              = True -- Is there a better way to do this?
          | (flagOff msg) `elem` (optFlags o)
              = False
          | otherwise -- Otherwise, default to verbosity
              = verbosity msg <= verbosityThreshold msg o
  in 
    filter p msgs