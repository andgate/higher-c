{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
  #-}
module Language.Hawk.Parse.Item where

import Control.Applicative
import Control.Lens
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Language.Hawk.Parse.Item.Types
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P


itemP :: ( MonadParser m
         , MonadState s m, HasLocalState s
         , MonadReader i m, HasGlobalInfo i
         )
      => m ItemPs
itemP = undefined