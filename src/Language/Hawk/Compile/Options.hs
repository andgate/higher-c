module Language.Hawk.Compile.Options where


data Command
    = Build String
    | Clean

data Opts = Opts
    { optCommand :: !Command

    -- Warning Flags
    , optInfoVerbosity :: !Int
    , optFlagInfoFileFound :: !Bool
    , optFlagInfoDirectoryFound :: !Bool
    , optFlagInfoFreshModuleFound :: !Bool
    , optFlagInfoModulePreserved :: !Bool

    -- Warning Flags
    , optWarnVerbosity :: !Int
    , optWarnFileIgnoredFlag :: !Bool
    , optWarnDirectoryIgnoredFlag :: !Bool
    , optWarnSymLinkIgnored :: !Bool
    }


defOpts :: Opts
defOpts =
  Opts
  { optCommand = Build ""

  -- Warning Flags
  , optInfoVerbosity = 1
  , optFlagInfoFileFound = False
  , optFlagInfoDirectoryFound = False
  , optFlagInfoFreshModuleFound = True
  , optFlagInfoModulePreserved = False

  -- Warning Flags
  , optWarnVerbosity = 1
  , optWarnFileIgnoredFlag = True
  , optWarnDirectoryIgnoredFlag  = True
  , optWarnSymLinkIgnored  = True
  }


class HasVerbosity a where
    verbosity :: a -> Int

class HasFlags a where
    flagged :: Opts -> a -> Bool
    flagged o a = not $ notFlagged o a

    notFlagged :: Opts -> a -> Bool
    notFlagged o a = not $ flagged o a


messageFilter :: (HasVerbosity a, HasFlags a) => Opts -> [a] -> [a]
messageFilter = undefined