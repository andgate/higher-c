module Language.Hawk.Parse.Lexer.Layout where

import Conduit
import Control.Monad (forever, mapM, when, unless)
import Control.Monad.Trans.State.Lazy (State, evalStateT, get, put, modify)
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (Region(..), Position(..))
import Safe (headDef)

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Text                      as Text

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
layout :: Conduit Token LayoutState Token
layout = do
    mayT <- await
    case mayT of
      (Just t@(Token c (Just p))) ->
        do
          preUpdate c p
          yield t
          postUpdate c
      
      Just t ->
        yield t
      
      Nothing ->
        return ()
  
preUpdate :: TokenClass -> Position -> Conduit Token LayoutState Token
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
  
  
postUpdate :: TokenClass -> Conduit Token LayoutState Token
postUpdate c =
  when (hasBlkTrig c && isNotBlkClass c) emitBlk
      
  where
    hasBlkTrig = isJust . Text.find (==':') . tokenClassToText
    isNotBlkClass (TokenMixfixBlkId _) = False
    isNotBlkClass _ = True
    
    -- Wait til a document (non-builtin) token arrives
    -- Passes builtin tokens along.
    -- Document tokens have position, so they represent a change in layout.
    awaitDocTok = do
      mt <- await
      case mt of
          Just t -> recieveTok t
          Nothing -> return Nothing

    recieveTok t@(Token c mp) =
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
            yield $ Token c (Just p)
          else
            error $ "\nExpected Indentation: " ++ show l ++
                    "\nActual Indentation: " ++ show i ++
                    "\nwith \"" ++ Text.unpack (tokenClassToText c) ++
                      "\" at " ++ show ln ++ ":" ++ show i

-- -----------------------------------------------------------------------------
-- Driver Helpers
  
closeInvalid :: Position -> Conduit Token LayoutState Token
closeInvalid p@(P _ i) = do
  l <- lift $ peekLay
  unless (isValid i l)
         (close >> closeInvalid p)
         
closeAll :: Position -> Conduit Token LayoutState Token
closeAll p = do
  l <- lift $ peekLay
  unless (l == defLay)
         (close >> closeAll p)
                    
coverBlock :: Position -> Conduit Token LayoutState Token
coverBlock p = do
  l <- lift $ peekLay
  case l of
      (Block i) -> open (LineFold i)
      _ -> return ()
      
      
handleEof :: TokenClass -> Position -> Conduit Token LayoutState Token
handleEof c p =
  when (c == TokenEof)
       (closeAll p)
  
      
open :: Layout -> Conduit Token LayoutState Token
open l = do
  lift $ pushLay l
  yield $ openTok l

close :: Conduit Token LayoutState Token
close = do
  l <- lift $ peekLay
  yield $ closeTok l
  lift $ popLay
  return ()
  
  
pushLayout :: Layout -> Conduit Token LayoutState Token
pushLayout = lift . pushLay

{-
popLayout :: Pipe Token Token LayoutState Layout
popLayout = lift $ popLay

peekLayout :: Pipe Token Token LayoutState Layout
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

    
openTok :: Layout -> Token
openTok l =
  case l of
      Block _ -> Token TokenBlk Nothing
      LineFold _ -> Token TokenLn Nothing   
    
closeTok :: Layout -> Token
closeTok l =
  case l of
      Block _ -> Token TokenBlk' Nothing
      LineFold _ -> Token TokenLn' Nothing
  
  
isValid :: Int -> Layout -> Bool
isValid i' (Block i) = i <= i'
-- Line fold should be layCol < curCol || (curLine == layLine && layCol <= curCol)
-- but this will do
isValid i' (LineFold i) = i < i'