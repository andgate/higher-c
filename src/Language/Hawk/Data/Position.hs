{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Data.Position where

import Data.Generics


-- | uniform representation of source file positions
data Position = Position { posFile    :: String                 -- ^ source file
                         , posRow     :: Int     -- ^ row (line)  in the original file. Affected by #LINE pragmas.
                         , posColumn  :: Int  -- ^ column in the preprocessed file. Inaccurate w.r.t. to the original
                                                             --   file in the presence of preprocessor macros.
                         }
              | NoPosition
                deriving (Eq, Ord, Typeable, Data)

instance Show Position where
  show (Position fname row col) = show fname ++ ":" ++ show row ++ ":" ++ show col
  show NoPosition               = "<no file>"

-- | class of type which aggregate a source code location
class Pos a where
    posOf :: a -> Position