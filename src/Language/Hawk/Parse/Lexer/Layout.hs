module Language.Hawk.Parse.Lexer.Layout where

import Conduit
import Control.Monad (Monad, when, unless)
import Control.Monad.Trans.State.Strict (StateT, get, put, modify)
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (Position(..))
import Safe (headDef)

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Text                      as Text

-- -----------------------------------------------------------------------------
-- Layout Types
data Container = 
  Block Int | LineFold Int
  deriving (Eq, Ord, Show)

defLay :: Container
defLay = Block 1

type Layout = StateT [Container]

defContainer :: [Container]
defContainer = []


-- -----------------------------------------------------------------------------
-- Conduit-based Layout Driver  
layout :: Conduit Token Layout Token
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
  
preUpdate :: TokenClass -> Position -> Conduit Token Layout Token
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
  
  
postUpdate :: TokenClass -> Conduit Token Layout Token
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
  
closeInvalid :: Position -> Conduit Token Layout Token
closeInvalid p@(P _ i) = do
  l <- lift $ peekLay
  unless (isValid i l)
         (close >> closeInvalid p)
         
closeAll :: Position -> Conduit Token Layout Token
closeAll p = do
  l <- lift $ peekLay
  unless (l == defLay)
         (close >> closeAll p)
                    
coverBlock :: Position -> Conduit Token Layout Token
coverBlock p = do
  l <- lift $ peekLay
  case l of
      (Block i) -> open (LineFold i)
      _ -> return ()
      
      
handleEof :: TokenClass -> Position -> Conduit Token Layout Token
handleEof c p =
  when (c == TokenEof)
       (closeAll p)
  
      
open :: Container -> Conduit Token Layout Token
open l = do
  lift $ pushLay l
  yield $ openTok l

close :: Conduit Token Layout Token
close = do
  l <- lift $ peekLay
  yield $ closeTok l
  lift $ popLay
  return ()
  
  
pushLayout :: Container -> Conduit Token Layout Token
pushLayout = lift . pushLay

{-
popLayout :: Pipe Token Token Layout Container
popLayout = lift $ popLay

peekLayout :: Pipe Token Token Layout Container
peekLayout = lift $ peekLay
-}

-- -----------------------------------------------------------------------------
-- Layout Helpers

getIndent :: Layout Int
getIndent = do
  lo <- headDef defLay <$> State.get
  case lo of
    Block i -> return i
    LineFold i -> return i
    
setIndent :: Int -> Layout ()
setIndent i = do
  l <- popLay
  let l' = case l of
              Block _ -> Block i
              LineFold _ -> LineFold i
  pushLay l'


pushLay :: Container -> Layout ()
pushLay l =
  modify (l:)

popLay :: Layout Container
popLay = do
  ls <- get
  case ls of
    [] -> return defLay
    l:ls' -> put ls' >> return l

peekLay :: Layout Container
peekLay = do
  ls <- get
  case ls of
    [] -> return defLay
    l:_ -> return l

    
openTok :: Container -> Token
openTok l =
  case l of
      Block _ -> Token TokenBlk Nothing
      LineFold _ -> Token TokenLn Nothing   
    
closeTok :: Container -> Token
closeTok l =
  case l of
      Block _ -> Token TokenBlk' Nothing
      LineFold _ -> Token TokenLn' Nothing
  
  
isValid :: Int -> Container -> Bool
isValid i' (Block i) = i <= i'
-- Line fold should be layCol < curCol || (curLine == layLine && layCol <= curCol)
-- but this will do
isValid i' (LineFold i) = i < i'