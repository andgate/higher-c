module Language.Hawk.Parse.Layout where

import Control.Monad (forever, mapM, when, unless)
import Control.Monad.Trans.State.Strict (State, evalStateT, get, put, modify)
import Lens.Micro.Mtl ((.=), (+=))
import Safe (headDef)
import Pipes (Pipe, await, yield, lift)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pipes.Prelude as Pipes
import qualified Language.Hawk.Parse.Lexer        as L

-- -----------------------------------------------------------------------------
-- Layout Types
data Layout = 
  Block Int | LineFold Int
  deriving (Eq, Ord, Show)

defLay :: Layout
defLay = Block 1

type LayoutState = State [Layout]

defState :: [Layout]
defState = []

blockTriggers :: [Text]
blockTriggers = [":=", "::", "do", "where", "of"]


-- -----------------------------------------------------------------------------
-- Pipe-based Layout Driver
layout :: Pipe L.Token L.Token LayoutState ()
layout = forever (await >>= update)

  
update :: L.Token -> Pipe L.Token L.Token LayoutState ()
update t = preUpdate t >> yield t >> postUpdate t
  
  
preUpdate :: L.Token -> Pipe L.Token L.Token LayoutState ()
preUpdate (L.Token c p) = do
  -- Close invalid layouts on the stack
  -- until the top of the stack is a
  -- valid or negative layout.
  closeInvalid p
  
  -- since a linefold might have just been closed,
  -- we may be left with a block layout.
  -- Block layouts must sit under a linefold.
  coverBlock p
  
  handleEof c p
  

postUpdate :: L.Token -> Pipe L.Token L.Token LayoutState ()
postUpdate (L.Token c _) =
  when isBlkTrig emitBlk
      
  where
    isBlkTrig = c `elem` blkTrigs
    blkTrigs = map L.TokenRsvp blockTriggers
    
    emitBlk = do 
      t@(L.Token _ p@(L.P _ i)) <- await
      l <- lift $ peekLay
      if isValid i l
          then do
            open (Block i) p
            open (LineFold i) p
            yield t
          else
            error $ "Expecting indent beyond " ++ show i

-- -----------------------------------------------------------------------------
-- Driver Helpers
  
closeInvalid :: L.Position -> Pipe L.Token L.Token LayoutState ()
closeInvalid p@(L.P _ i) = do
  l <- lift $ peekLay
  unless (isValid i l)
         (close p >> closeInvalid p)
         
closeAll :: L.Position -> Pipe L.Token L.Token LayoutState ()
closeAll p = do
  l <- lift $ peekLay
  unless (l == defLay)
         (close p >> closeAll p)
                    
coverBlock :: L.Position -> Pipe L.Token L.Token LayoutState ()
coverBlock p = do
  l <- lift $ peekLay
  case l of
      (Block i) -> open (LineFold i) p
      _ -> return ()
      
      
handleEof :: L.TokenClass -> L.Position -> Pipe L.Token L.Token LayoutState ()
handleEof c p =
  when (c == L.TokenEof)
       (closeAll p)
  
      
open :: Layout -> L.Position -> Pipe L.Token L.Token LayoutState ()
open l p = do
  lift $ pushLay l
  yield $ openTok l p

close :: L.Position -> Pipe L.Token L.Token LayoutState ()
close p = do
  close' p
  lift $ popLay >> return ()
  
close' :: L.Position -> Pipe L.Token L.Token LayoutState ()
close' p = do
  l <- lift $ peekLay
  yield $ closeTok l p
  
pushLayout :: Layout -> Pipe L.Token L.Token LayoutState ()
pushLayout = lift . pushLay

popLayout :: Pipe L.Token L.Token LayoutState Layout
popLayout = lift $ popLay

peekLayout :: Pipe L.Token L.Token LayoutState Layout
peekLayout = lift $ peekLay

-- -----------------------------------------------------------------------------
-- LayoutState Helpers

getIndent :: LayoutState Int
getIndent = do
  lo <- headDef defLay <$> State.get
  case lo of
    Block i -> return i
    LineFold i -> return i
    
setIndent :: Int -> LayoutState ()
setIndent i = do
  l <- popLay
  let l' = case l of
              Block _ -> Block i
              LineFold _ -> LineFold i
  pushLay l'


pushLay :: Layout -> LayoutState ()
pushLay l =
  modify (l:)

popLay :: LayoutState Layout
popLay = do
  ls <- get
  case ls of
    [] -> return defLay
    l:ls' -> put ls' >> return l

peekLay :: LayoutState Layout
peekLay = do
  ls <- get
  case ls of
    [] -> return defLay
    l:_ -> return l

    
openTok :: Layout -> L.Position -> L.Token
openTok l p =
  case l of
      Block _ -> L.Token L.TokenBlk p
      LineFold _ -> L.Token L.TokenLn p    
    
closeTok :: Layout -> L.Position -> L.Token
closeTok l p =
  case l of
      Block _ -> L.Token L.TokenBlk' p
      LineFold _ -> L.Token L.TokenLn' p
  
  
isValid :: Int -> Layout -> Bool
isValid i' (Block i) = i <= i'
-- Line fold should be layCol < curCol || (curLine == layLine && layCol <= curCol)
-- but this will do
isValid i' (LineFold i) = i < i'