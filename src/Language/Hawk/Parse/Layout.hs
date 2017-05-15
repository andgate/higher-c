module Language.Hawk.Parse.Layout where

import Control.Monad (forever, mapM, when, unless)
import Control.Monad.Trans.State.Lazy (State, evalStateT, get, put, modify)
import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Language.Hawk.Report.Region (Region(..), Position(..))
import Lens.Micro.Mtl ((.=), (+=))
import Conduit
import Safe (headDef)

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Text.Lazy                   as Text
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


-- -----------------------------------------------------------------------------
-- Conduit-based Layout Driver  
layout :: Conduit L.Token LayoutState L.Token
layout = do
    mayT <- await
    case mayT of
      (Just t@(L.Token c (Just p))) ->
        do
          preUpdate c p
          yield t
          postUpdate c
      
      Just t ->
        yield t
      
      Nothing ->
        return ()
  
preUpdate :: L.TokenClass -> Position -> Conduit L.Token LayoutState L.Token
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
  
  
postUpdate :: L.TokenClass -> Conduit L.Token LayoutState L.Token
postUpdate c =
  when (hasBlkTrig c && isNotBlkClass c) emitBlk
      
  where
    hasBlkTrig = isJust . Text.find (==':') . L.tokenClassToText
    isNotBlkClass (L.TokenMixfixBlkId _) = False
    isNotBlkClass _ = True
    
    -- Wait til a document (non-builtin) token arrives
    -- Passes builtin tokens along.
    -- Document tokens have position, so they represent a change in layout.
    awaitDocTok = do
      mt <- await
      case mt of
          Just t -> recieveTok t
          Nothing -> return Nothing

    recieveTok t@(L.Token c mp) =
      case mp of
          Just p -> preUpdate c p >> return (Just (c,p))
          Nothing -> yield t >> awaitDocTok
    
    emitBlk = do
      mayDocTok <- awaitDocTok
      case mayDocTok of 
          Just docTok -> handleDocTok docTok
          Nothing -> return ()

    handleDocTok (c, p@(P ln i)) = do
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
  
closeInvalid :: Position -> Conduit L.Token LayoutState L.Token
closeInvalid p@(P _ i) = do
  l <- lift $ peekLay
  unless (isValid i l)
         (close >> closeInvalid p)
         
closeAll :: Position -> Conduit L.Token LayoutState L.Token
closeAll p = do
  l <- lift $ peekLay
  unless (l == defLay)
         (close >> closeAll p)
                    
coverBlock :: Position -> Conduit L.Token LayoutState L.Token
coverBlock p = do
  l <- lift $ peekLay
  case l of
      (Block i) -> open (LineFold i)
      _ -> return ()
      
      
handleEof :: L.TokenClass -> Position -> Conduit L.Token LayoutState L.Token
handleEof c p =
  when (c == L.TokenEof)
       (closeAll p)
  
      
open :: Layout -> Conduit L.Token LayoutState L.Token
open l = do
  lift $ pushLay l
  yield $ openTok l

close :: Conduit L.Token LayoutState L.Token
close = do
  l <- lift $ peekLay
  yield $ closeTok l
  lift $ popLay
  return ()
  
  
pushLayout :: Layout -> Conduit L.Token LayoutState L.Token
pushLayout = lift . pushLay

{-
popLayout :: Pipe L.Token L.Token LayoutState Layout
popLayout = lift $ popLay

peekLayout :: Pipe L.Token L.Token LayoutState Layout
peekLayout = lift $ peekLay
-}

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