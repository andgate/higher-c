{-# LANGUAGE  FlexibleContexts #-}
module Language.Hawk.NameGen (newName) where

import Control.Monad
import Control.Monad.Gen
import Data.Text (Text, pack)


-- When under a monad-gen, new name will select the next var from a
-- sequence of "a1, b1, ..., a2, b2, .., zn" where n is some integer.
newName :: MonadGen Int m
        => m Text
newName = do
  n <- gen
  return $ pack (letters !! n)


-- Letters is an infinite list that hopefully gets memoized
-- but doesn't leak. There is probably a better way to
-- generate variable names.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']
  
