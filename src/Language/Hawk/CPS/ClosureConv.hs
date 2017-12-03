{-# LANGUAGE  FlexibleContexts
            , LambdaCase
  #-}
module Language.Hawk.CPS.ClosureConv where


import Control.Lens
import Control.Monad.Gen
import Data.Text (Text)
import Language.Hawk.CPS.Syntax


closureconv :: (MonadGen Int m)
    => Text -> CExp -> m CExp
closureconv def_n = \case
  CLet n b e' -> undefined
  CIf p a b -> undefined
  CJump a a -> undefined
