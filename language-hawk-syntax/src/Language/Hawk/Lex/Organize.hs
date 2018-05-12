{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , TemplateHaskell
           , DataKinds
           , KindSignatures
           , GADTs
  #-}
module Language.Hawk.Lex.Organize where

import Control.Lens
import Control.Monad (when, unless, void)
import Control.Monad.State.Strict (State, evalState)
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location
import Safe (headDef)
import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Z

import qualified Data.Map.Strict as Map


-- -----------------------------------------------------------------------------
-- Cells

data CellType
  = Block
  | Frame
  | LineFold
  deriving (Eq, Show)

data Cell =
  Cell
  { _cellPos  :: Position
  , _cellType :: CellType
  } deriving (Eq, Show)

makeLenses ''Cell

defCell :: Cell
defCell = Cell (P 0 0) Block

-- -----------------------------------------------------------------------------
-- Cell Layout

data LayoutState =
    LayoutState
    { _layCells  :: [Cell]
    , _layToks   :: Zipper Token
    }

makeLenses ''LayoutState

type Layout = State LayoutState

mkLayout :: [Token] -> LayoutState
mkLayout input =
  LayoutState [] (Z.fromList input)


-- -----------------------------------------------------------------------------
-- Organize Tokens

-- | These tokens will trigger a block open on the next token.
blkTriggers :: [TokenClass]
blkTriggers = [TokenRsvp "do", TokenRsvp "has"]

-- | Tokens to open a frame 
frameOpenTriggers :: [TokenClass]
frameOpenTriggers = [TokenRsvp "let"]

-- | Tokens that close a frame
frameCloseTriggers :: [TokenClass]
frameCloseTriggers = [TokenRsvp "in"]

-- | Organize tokens with Blocks and Linefolds
organize :: [Token] -> [Token]
organize =
  evalState layoutDriver . mkLayout


-- -----------------------------------------------------------------------------
-- Layout Driver

layoutDriver :: Layout [Token]
layoutDriver = do
  openCell Block
  openCell LineFold
  go
  where
    go = do
      ts <- use layToks
      if Z.endp ts then do
        closeCellStack
        uses layToks Z.toList
        
      else do
        updateCursor
        go


updateCursor :: Layout ()
updateCursor = do
      t <- cursorToken

      repairBlock

      -- Open a Block if the cursor requires it.
      when (t^.tokClass `elem` frameCloseTriggers)
           exitFrame
      
      nextToken

      -- Open a Block if the cursor requires it.
      when (t^.tokClass `elem` blkTriggers)
           (openCell Block)

      when (t^.tokClass `elem` frameOpenTriggers)
           (openCell Frame)

      -- Close any cells the cursor has left.
      closeEscapedCells


-- | Shift right to the next token
nextToken :: Layout ()
nextToken =
  layToks %= Z.right


-- | Close cells that the current cursor has left.
closeEscapedCells :: Layout ()
closeEscapedCells = do
  q <- cursorInCell
  unless q (closeCell >> closeEscapedCells)


-- | If the top cell is a block, cover with a linefold
repairBlock :: Layout ()
repairBlock = do
  (Cell _ ty) <- peekCell
  case ty of
    Block    -> openCell LineFold
    Frame    -> return ()
    LineFold -> return ()
    

-- | Test if the cursor is within the current cell.
cursorInCell :: Layout Bool
cursorInCell = do
  i <- cursorIndent
  ln <- cursorIndent
  cl <- peekCell
  return $ inCell ln i cl


-- | Predicate for testing if a cursor is in the current cell.
inCell :: Int  -- Line to test 
       -> Int  -- Indent to test
       -> Cell -- Cell to test indent on
       -> Bool -- True is indent is within cell, false otherwise
inCell ln i (Cell (P cln ci) ct) =
  case ct of
    Block    ->  ci <= i
    Frame    ->  ci <= i
    LineFold ->  (ci < i) && (ln /= cln)
    


-- -----------------------------------------------------------------------------
-- Cursor Helpers

cursorToken :: Layout Token
cursorToken = do
  ts <- use layToks
  if Z.endp ts then
    lastCursorTokenUnsafe
  else
    cursorTokenUnsafe


lastCursorTokenUnsafe :: Layout Token
lastCursorTokenUnsafe =
  uses layToks (Z.cursor . Z.left)
  

cursorTokenUnsafe :: Layout Token
cursorTokenUnsafe = 
  uses layToks Z.cursor 


cursorIndent :: Layout Int
cursorIndent = do
  (^. tokLoc . locReg . regStart . posColumn) <$> cursorToken


cursorLine :: Layout Int
cursorLine = do
  (^. tokLoc . locReg . regStart . posLine) <$> cursorToken


cursorFile :: Layout FilePath
cursorFile = do
  (^. tokLoc . locPath) <$> cursorToken


-- -----------------------------------------------------------------------------
-- Cell Helpers

getCellIndent :: Layout Int
getCellIndent =
  (^. cellPos . posColumn) <$> peekCell


pushCell :: Cell -> Layout ()
pushCell l =
  layCells %= (l:)


popCell :: Layout Cell
popCell = do
  cn <- peekCell
  layCells %= tail
  return cn


peekCell :: Layout Cell
peekCell = 
  uses layCells (headDef defCell)

-- | Open a cell, inserting a token at the cursor.
openCell :: CellType -> Layout ()
openCell ty = do
  -- Get env info
  fp <- cursorFile
  ln <- cursorLine
  i <- cursorIndent

  -- Build cell and token
  let p = P ln i
      cell = Cell p ty
      cellOpenTok = mkCellOpenTok cell fp

  -- Push new cell, insert token
  pushCell cell
  layToks %= Z.right . Z.insert cellOpenTok


-- | Close a cell, inserting a token at the cursor.
closeCell :: Layout Cell
closeCell = do
  -- Fetch some info
  c@(Cell _ ty) <- popCell -- Conviently destroy top cell
  fp <- cursorFile
  ln <- cursorLine
  i <- cursorIndent

  -- Build and insert a closing token
  let p = P ln i
      cellClose = Cell p ty
      cellCloseTok = mkCellCloseTok cellClose fp
  layToks %= Z.right . Z.insert cellCloseTok
  return cellClose


exitFrame :: Layout ()
exitFrame = do
  (Cell _ ty) <- closeCell
  case ty of
    Frame -> return ()
    _ -> exitFrame


-- | Close the entire stack of cells at the cursor.
closeCellStack :: Layout ()
closeCellStack = do
  cl <- peekCell
  unless (cl == defCell)
         (closeCell >> closeCellStack)


-- | Convert some cell to an open token
mkCellOpenTok :: Cell -> FilePath -> Token
mkCellOpenTok (Cell p ty) fp =
  case ty of
      Block -> Token TokenBlk "" loc
      Frame -> Token TokenBlk "" loc
      LineFold -> Token TokenLn "" loc
  where loc = Loc fp (R p p)

-- | Convert some cell to a closed token
mkCellCloseTok :: Cell -> FilePath -> Token
mkCellCloseTok (Cell p ty) fp =
  case ty of
      Block -> Token TokenBlk' "" loc
      Frame -> Token TokenBlk' "" loc
      LineFold -> Token TokenLn' "" loc
  where loc = Loc fp (R p p)