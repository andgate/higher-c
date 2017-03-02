module Language.Hawk.Parse.Layout where

import Control.Monad (forever, mapM, when, unless)
import Control.Monad.Trans.State.Strict (State, evalStateT, get, put, modify)
import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Language.Hawk.Report.Region (Region(..), Position(..))
import Lens.Micro.Mtl ((.=), (+=))
import Pipes (Pipe, await, yield, lift)
import Safe (headDef)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.Lazy                   as Text
import qualified Language.Hawk.Parse.Lexer        as L
import qualified Pipes.Prelude                    as Pipes

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


-- -----------------------------------------------------------------------------
-- Pipe-based Layout Driver
layout :: Pipe L.Token L.Token LayoutState ()
layout = forever (await >>= update)

  
update :: L.Token -> Pipe L.Token L.Token LayoutState ()
update t@(L.Token c (Just p)) = do
    preUpdate c p
    yield t
    postUpdate c

update t = yield t
  
preUpdate :: L.TokenClass -> Position -> Pipe L.Token L.Token LayoutState ()
preUpdate c p = do
  -- Close invalid layouts on the stack
  -- until the top of the stack is a
  -- valid or negative layout.
  closeInvalid p
  
  -- since a linefold might have just been closed,
  -- we may be left with a block layout.
  -- Block layouts must sit under a linefold.
  coverBlock p
  
  handleEof c p
  
  
postUpdate :: L.TokenClass -> Pipe L.Token L.Token LayoutState ()
postUpdate c =
  when (hasBlkTrig c && isNotBlkClass c) emitBlk
      
  where
    hasBlkTrig = isJust . Text.find (==':') . L.tokenClassToText
    isNotBlkClass (L.TokenMixfixBlkId _) = False
    isNotBlkClass _ = True
    
    -- Wait til a document (non-builtin) token arrives
    -- Passes builtin tokens along.
    awaitDocTok = do
      t@(L.Token c mp) <- await
      case mp of
        Just p -> preUpdate c p >> return (c,p)
        Nothing -> yield t >> awaitDocTok
    
    emitBlk = do 
      (c, p@(P ln i)) <- awaitDocTok
      l <- lift $ peekLay
      if isValid i l
          then do
            open (Block i)
            open (LineFold i)
            yield $ L.Token c (Just p)
          else
            error $ "\nExpected Indentation: " ++ show l ++
                    "\nActual Indentation: " ++ show i ++
                    "\nwith \"" ++ Text.unpack (L.tokenClassToText c) ++
                      "\" at " ++ show ln ++ ":" ++ show i

-- -----------------------------------------------------------------------------
-- Driver Helpers
  
closeInvalid :: Position -> Pipe L.Token L.Token LayoutState ()
closeInvalid p@(P _ i) = do
  l <- lift $ peekLay
  unless (isValid i l)
         (close >> closeInvalid p)
         
closeAll :: Position -> Pipe L.Token L.Token LayoutState ()
closeAll p = do
  l <- lift $ peekLay
  unless (l == defLay)
         (close >> closeAll p)
                    
coverBlock :: Position -> Pipe L.Token L.Token LayoutState ()
coverBlock p = do
  l <- lift $ peekLay
  case l of
      (Block i) -> open (LineFold i)
      _ -> return ()
      
      
handleEof :: L.TokenClass -> Position -> Pipe L.Token L.Token LayoutState ()
handleEof c p =
  when (c == L.TokenEof)
       (closeAll p)
  
      
open :: Layout -> Pipe L.Token L.Token LayoutState ()
open l = do
  lift $ pushLay l
  yield $ openTok l

close :: Pipe L.Token L.Token LayoutState ()
close = do
  l <- lift $ peekLay
  yield $ closeTok l
  lift $ popLay
  return ()
  
  
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

    
openTok :: Layout -> L.Token
openTok l =
  case l of
      Block _ -> L.Token L.TokenBlk Nothing
      LineFold _ -> L.Token L.TokenLn Nothing   
    
closeTok :: Layout -> L.Token
closeTok l =
  case l of
      Block _ -> L.Token L.TokenBlk' Nothing
      LineFold _ -> L.Token L.TokenLn' Nothing
  
  
isValid :: Int -> Layout -> Bool
isValid i' (Block i) = i <= i'
-- Line fold should be layCol < curCol || (curLine == layLine && layCol <= curCol)
-- but this will do
isValid i' (LineFold i) = i < i'