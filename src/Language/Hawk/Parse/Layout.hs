module Language.Hawk.Parse.Layout where

import Control.Monad (forever, mapM)
import Control.Monad.Trans.State.Strict (State)
import Lens.Micro.Mtl ((.=), (+=))
import Safe (headMay)
import Pipes (Pipe, await, yield, lift)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pipes.Prelude as Pipes
import qualified Language.Hawk.Parse.Lexer        as L

data Layout = 
  Block Int | LineFold Int
  deriving (Eq, Ord, Show)


type LayoutState = State [Layout]

getIndent :: LayoutState Int
getIndent = do
  lo <- headMay <$> State.get
  case lo of
    Nothing -> return 0
    Just (Block i) -> return i
    Just (LineFold i) -> return i

    
closeTok :: Layout -> L.TokenClass
closeTok l =
  case l of
      Block _ -> L.TokenBlk'
      LineFold _ -> L.TokenLn'

openTok :: Layout -> L.TokenClass
openTok l =
  case l of
      Block _ -> L.TokenBlk
      LineFold _ -> L.TokenLn



layout :: Pipe L.Token L.Token LayoutState ()
layout = forever $ do
  t <- await
  ts <- lift $ update t
  mapM yield ts

  
update :: L.Token -> LayoutState [L.Token]
update t = do
  tsPre <- preUpdate t
  tsPost <- postUpdate t

  return $ tsPre ++ [t] ++ tsPost
  
  
preUpdate :: L.Token -> LayoutState  [L.Token]
preUpdate t = do
  -- take layouts from the stack that are no longer valid
  -- and generate a list of tokens to close those layouts.
  -- check to see if linefold is invalidated
  -- check to see if block is invalidated
  -- repeat, poping and converting layouts to tokens.
  return []

postUpdate :: L.Token -> LayoutState [L.Token]
postUpdate t = do
  -- Check to see if the token needs a layout yielded afterwards
  -- and collect any layouts that are needed.
  return []