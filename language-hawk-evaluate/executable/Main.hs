{-# Language TemplateHaskell
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
  #-}
module Main where

import Prelude hiding (lex)

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops (untilM_)
import Data.Bifunctor
import Data.Bitraversable
import Data.Either
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Hawk.Parse
import Language.Hawk.Lex (lex)
import Language.Hawk.Lex.Token (Token)
import Language.Hawk.Rename
import Language.Hawk.Typecheck
import System.IO (hFlush, stdout)

import qualified Language.Hawk.Syntax.Source as S
import qualified Language.Hawk.Syntax.Suspension as Ss
import qualified Data.Text.IO as T


data ReplState
  = ReplState
    { _replQuit :: Bool
    , _replDecls :: [S.Decl]
    , _replHistory :: [String]
    }

makeLenses ''ReplState

initialReplState :: ReplState
initialReplState =
  ReplState
    { _replQuit = False
    , _replDecls = []
    , _replHistory = []
    }


newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Functor, Applicative, Monad, MonadState ReplState, MonadIO)

runRepl :: Repl a -> IO a
runRepl m = evalStateT (unRepl m) initialReplState

main :: IO ()
main = runRepl (loadPrelude >> repl)

repl :: Repl ()
repl = untilM_ (read' >>= traverse eval') (use replQuit)

read' :: Repl (Maybe S.Decl)
read' = do
  ln <- liftIO (T.putStr "> " >> hFlush stdout >> T.getLine)
  r <- bitraverse (\err -> liftIO ( putDoc (pretty err) >> T.putStr "\n"))
                  return
                  (parseDecl "repl" ln)
  return $ eitherToMaybe r

eval' :: S.Decl -> Repl ()
eval' d = do
  liftIO $ do
    putDoc $ "parsed:" <+> pretty d
    T.putStr "\n"
  -- suspend and pretty print suspended

loadPrelude :: Repl ()
loadPrelude = return ()