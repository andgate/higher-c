{-# LANGUAGE  GeneralizedNewtypeDeriving
  #-}
module Language.Hawk.Parse.Item.Monad where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Default.Class
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Item.Types
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Parse.Helpers (ParserOpTable)

import qualified Text.Megaparsec.Prim   as P
import qualified Text.Megaparsec.Expr   as P
import qualified Text.Megaparsec.Error  as P

-- Monad stack for item parsing
newtype ItemParserT m a = ItemParserT { unItemParserT :: ReaderT GlobalInfo (P.ParsecT P.Dec [Token] m) a }
    deriving (Functor, Monad, Applicative, Alternative, MonadPlus, MonadReader GlobalInfo, P.MonadParsec P.Dec [Token])


runItemParserT :: Monad m => ItemParserT m a -> FilePath -> ParserOpTable -> [Token] -> m (Either (P.ParseError Token P.Dec) a)
runItemParserT m fp optab ts
  = P.runParserT (runReaderT (unItemParserT m) ginfo) fp ts
  where
    ginfo = GlobalInfo fp optab
