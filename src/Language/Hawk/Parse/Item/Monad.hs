{-# LANGUAGE  GeneralizedNewtypeDeriving
  #-}
module Language.Hawk.Parse.Item.Monad where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Default.Class
import Language.Hawk.Parse.Item.Types
import Language.Hawk.Parse.Lexer.Token

import qualified Text.Megaparsec.Prim   as P
import qualified Text.Megaparsec.Error  as P

-- Monad stack for item parsing
newtype ItemParserT m a = ItemParserT { unItemParserT :: StateT LocalState (ReaderT GlobalInfo (P.ParsecT P.Dec [Token] m)) a }
    deriving (Functor, Monad, Applicative, Alternative, MonadPlus, MonadState LocalState, MonadReader GlobalInfo, P.MonadParsec P.Dec [Token])


runItemParserT :: Monad m => ItemParserT m a -> GlobalInfo -> [Token] -> m (Either (P.ParseError Token P.Dec) a)
runItemParserT m i ts
  = P.runParserT (runReaderT (evalStateT (unItemParserT m) def) i) (i^.gFilePath) ts
