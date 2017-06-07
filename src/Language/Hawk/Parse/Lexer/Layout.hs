{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , TemplateHaskell
  #-}
module Language.Hawk.Parse.Lexer.Layout where

import Conduit
import Control.Lens
import Control.Monad (Monad, when, unless, void, (>=>))
import Control.Monad.State.Strict (StateT)
import Data.Maybe (isJust)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (Region, Position)
import Safe (headDef)

import qualified Control.Monad.State.Strict   as State
import qualified Language.Hawk.Report.Region  as R
import qualified Data.Text                    as Text


-- -----------------------------------------------------------------------------
-- Cell Type

data CellType =
    Block
  | LineFold
  deriving (Eq, Ord, Show)

data Cell =
  Cell
  { _cellIndent :: !Int
  , _cellType :: CellType
  } deriving (Eq, Ord, Show)

makeLenses ''Cell

defCell :: Cell
defCell = Cell 0 Block

-- -----------------------------------------------------------------------------
-- Layout Types

data LayoutState =
    LayoutState
    { _layFilePath :: FilePath
    , _layRegion :: Region
    , _layStack :: [Cell]
    , _blkTriggered :: Bool
    } deriving (Eq, Ord, Show)

makeLenses ''LayoutState

type Layout a = forall m. Monad m => StateT LayoutState m a

type LayoutConduit = forall m. Monad m => Conduit Token (StateT LayoutState m) Token

defLayout :: LayoutState
defLayout = LayoutState  "" mempty [defCell] False


-- -----------------------------------------------------------------------------
-- Layout Driver  
layout :: Monad m => Conduit Token m Token
layout =
    evalStateC defLayout $
      awaitForever $ \t -> do
        lift $ updateLocation t
        handleTok t

handleTok :: Token -> LayoutConduit
handleTok t@(Token tc _ _ _)
  | tc == TokenEof = closeStack

  | otherwise = do
      -- Blocks triggered on last token need to be handled
      emitBlk <- lift $ use blkTriggered
      when emitBlk $ do
        lift $ blkTriggered .= False
        open Block

      closeInvalid
      yield t

      -- Colons trigger block emission for the next token
      when (tc == TokenRsvp ":")
           (lift $ blkTriggered .= True)


-- -----------------------------------------------------------------------------
-- Driver Helpers
  

closeStack :: LayoutConduit
closeStack = do
  cl <- lift peekCell
  unless (cl == defCell)
         (close >> closeStack)


closeInvalid :: LayoutConduit
closeInvalid = do
  go =<< lift getCurrIndent
  fillBlock
  where
    go i = do
      cl <- lift peekCell
      unless (isValid i cl)
             (close >> go i)


fillBlock :: LayoutConduit
fillBlock = do
  (Cell _ ct) <- lift peekCell
  when (ct == Block)
       (open LineFold)
  
      
open :: CellType -> LayoutConduit
open ct = do
  fp <- lift $ use layFilePath
  r <- lift $ use layRegion
  i <- lift getCurrIndent

  let cl = Cell i ct
  pushLayout cl
  yield $ openTok fp r cl

close :: LayoutConduit
close = do
  l <- lift peekCell
  fp <- lift $ use layFilePath
  r <- lift $ use layRegion
  yield $ closeTok fp r l
  lift $ void popCell
  
  
pushLayout :: Cell -> LayoutConduit
pushLayout = lift . pushCell

{-
popLayout :: Pipe Token Token Layout Cell
popLayout = lift . popLay

peekLayout :: Pipe Token Token Layout Cell
peekLayout = lift . peekLay
-}


-- -----------------------------------------------------------------------------
-- Layout Helpers

updateLocation :: Token -> Layout ()
updateLocation (Token _ _ fp r) = do
    layFilePath .= fp
    layRegion .= r

getCellIndent :: Layout Int
getCellIndent =
  _cellIndent <$> peekCell

getCurrIndent :: Layout Int
getCurrIndent =
  use $ layRegion . R.regStart . R.posColumn
    
setIndent :: Int -> Layout ()
setIndent i =
  layStack . ix 0 . cellIndent .= i


pushCell :: Cell -> Layout ()
pushCell l =
  layStack %= (l:)

popCell :: Layout Cell
popCell = do
  cn <- peekCell
  layStack %= tail
  return cn

peekCell :: Layout Cell
peekCell = 
  uses layStack head

    
openTok :: FilePath -> Region -> Cell -> Token
openTok fp r c =
  case c ^. cellType of
      Block -> Token TokenBlk "" fp r
      LineFold -> Token TokenLn "" fp r


closeTok :: FilePath -> Region -> Cell -> Token
closeTok fp r c =
  case c ^. cellType of
      Block -> Token TokenBlk' "" fp r
      LineFold -> Token TokenLn' "" fp r
  
  
isValid :: Int -> Cell -> Bool
isValid i (Cell ci ct) =
  case ct of
    Block -> 
      ci <= i
    
    LineFold ->
      ci < i