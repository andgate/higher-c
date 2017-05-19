{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Language.Hawk.Parse.Lexer.Layout where

import Conduit
import Control.Monad (Monad, when, unless)
import Control.Monad.State.Strict (StateT, get, put, modify)
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (Region, Position)
import Safe (headDef)

import qualified Control.Monad.State.Strict   as State
import qualified Language.Hawk.Report.Region  as R
import qualified Data.Text                    as Text

-- -----------------------------------------------------------------------------
-- Layout Types
data Container = 
  Block Int | LineFold Int
  deriving (Eq, Ord, Show)

defLay :: Container
defLay = Block 0

type Layout a = forall m. Monad m => StateT [Container] m a

type LayoutConduit = forall m. Monad m => Conduit Token (StateT [Container] m) Token

defLayout :: [Container]
defLayout = []


-- -----------------------------------------------------------------------------
-- Layout Driver  
layout :: Monad m => Conduit Token m Token
layout = evalStateC defLayout (awaitForever go)
  where go t@(Token c (Just r)) = do
            preUpdate c r
            yield t
            postUpdate c

        go t = yield t
  
preUpdate :: TokenClass -> Region -> LayoutConduit
preUpdate c (R.R p _) = do
  -- Close invalid layouts on the stack
  -- until the top of the stack is a
  -- valid or negative layout.
  closeInvalid p
  
  -- since a linefold might have just been closed,
  -- we may be left with a block layout.
  -- Block layouts must sit under a linefold.
  coverBlock p

  handleEof c p
  
  
postUpdate :: TokenClass -> LayoutConduit
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

    recieveTok t@(Token c (Just p)) =
        preUpdate c p >> return (Just (c,p))
    
    recieveTok t@(Token c Nothing) =
        yield t >> awaitDocTok
    
    emitBlk = do
      mayDocTok <- awaitDocTok
      case mayDocTok of 
          Just docTok -> handleDocTok docTok
          Nothing -> return ()

    handleDocTok (c, r@(R.R (R.P ln i) _)) = do
      l <- lift $ peekLay
      if isValid i l
          then do
            open (Block i)
            open (LineFold i)
            yield $ Token c (Just r)
          else
            error $ "\nExpected Indentation: " ++ show l ++
                    "\nActual Indentation: " ++ show i ++
                    "\nwith \"" ++ Text.unpack (tokenClassToText c) ++
                      "\" at " ++ show ln ++ ":" ++ show i

-- -----------------------------------------------------------------------------
-- Driver Helpers
  
closeInvalid :: Position -> LayoutConduit
closeInvalid p@(R.P _ i) = do
  l <- lift $ peekLay
  unless (isValid i l)
         (close >> closeInvalid p)
         
closeAll :: Position -> LayoutConduit
closeAll p = do
  l <- lift $ peekLay
  unless (l == defLay)
         (close >> closeAll p)
                    
coverBlock :: Position -> LayoutConduit
coverBlock p = do
  l <- lift $ peekLay
  case l of
      (Block i) -> open (LineFold i)
      _ -> return ()
      
      
handleEof :: TokenClass -> Position -> LayoutConduit
handleEof c p =
  when (c == TokenEof)
       (closeAll p)
  
      
open :: Container -> LayoutConduit
open l = do
  lift $ pushLay l
  yield $ openTok l

close :: LayoutConduit
close = do
  l <- lift $ peekLay
  yield $ closeTok l
  lift $ popLay
  return ()
  
  
pushLayout :: Container -> LayoutConduit
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