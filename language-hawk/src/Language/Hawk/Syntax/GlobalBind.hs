module Language.Hawk.Syntax.GlobalBind where

import Bound
import Bound.Var
import Bound.Scope
import Data.Foldable
import Data.Text (Text)


class GlobalBound t where
  -- Perform substitution on both vars and globals inside a stucture
  bound :: GlobalBind e
        => (v -> e v')
        -> (Text -> e v')
        -> t e v
        -> t e v'

{-
instance (GlobalBound b, Functor f b) => GlobalBound f b where
  bound f g bs = bound f g <$> bs
-}

instance GlobalBound (Scope b) where
  bound f g (Scope s) = Scope $ bind
                          (unvar (pure . B) $ pure . F . bind f g)
                          (pure . F . g)
                          s


class Monad e => GlobalBind e where
  global :: Text -> e v
  -- Perform substitution on both variables and globals
  bind :: (v -> e v')
       -> (Text -> e v')
       -> e v
       -> e v'