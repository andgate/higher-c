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
import Language.Hawk.Syntax.Location (Loc(..), Region, locReg, regStart, posColumn)
import Safe (headDef)
import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Z

import qualified Data.Map.Strict as Map
import qualified Language.Hawk.Syntax.Location  as L


-- -----------------------------------------------------------------------------
-- Cells

data CellType
  = Block
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
-- Cell Layout

data LayoutState =
    LayoutState
    { _layFilePath  :: FilePath
    , _layRegion    :: Region
    , _layCells     :: [Cell]
    , _layToks      :: Zipper Token
    }

makeLenses ''LayoutState

type Layout = State LayoutState

mkLayout :: [Token] -> LayoutState
mkLayout input =
  LayoutState  "" mempty [defCell] (Z.fromList input)


-- -----------------------------------------------------------------------------
-- Organize Tokens

-- | These tokens will trigger a block
blkTriggers :: [TokenClass]
blkTriggers = [TokenRsvp "do", TokenRsvp "has", TokenRsvp "let"]

-- | Organize tokens with Blocks and Linefolds
organize :: [Token] -> [Token]
organize =
  evalState layoutDriver . mkLayout

-- -----------------------------------------------------------------------------
-- Layout Driver

layoutDriver :: Layout [Token]
layoutDriver = do
  openCell LineFold
  driver

  where
    driver = do
      ts <- use layToks
      if Z.endp ts || Z.emptyp ts then do
        closeCellStack
        uses layToks Z.toList
        
      else do
        updateCursor
        driver


updateCursor :: Layout ()
updateCursor = do
      -- Close any cells the cursor has left.
      closeEscapedCells

      t <- Z.cursor <$> use layToks
      -- Open a Block if the cursor requires it.
      when (t^.tokClass `elem` blkTriggers)
           (openCell Block >> openCell LineFold)

      -- Grab next cursor.
      layToks %= Z.right
      


-- | Close cells that the current cursor has left.
closeEscapedCells :: Layout ()
closeEscapedCells = do
  q <- cursorInCell
  unless q (closeCell >> closeEscapedCells)


-- | Test if the cursor is within the current cell.
cursorInCell :: Layout Bool
cursorInCell = do
  i <- cursorIndent
  cl <- peekCell
  return $ inCell i cl


-- | Predicate for testing if a cursor is in the current cell.
inCell :: Int  -- Indent to test
        -> Cell -- Cell to test indent on
        -> Bool -- True is indent is within cell, false otherwise
inCell i (Cell ci ct) =
  case ct of
    Block -> 
      ci <= i
    
    LineFold ->
      ci < i


-- -----------------------------------------------------------------------------
-- Cursor Indent Helpers

-- | Cursor | --
updateIndent :: Token -> Layout ()
updateIndent (Token _ _ (Loc fp r)) = do
    layFilePath .= fp
    layRegion .= r


cursorIndent :: Layout Int
cursorIndent =
  use $ layRegion . L.regStart . L.posColumn


-- | Cell | --
cursorCellIndent :: Layout Int
cursorCellIndent =
  _cellIndent <$> peekCell


setCellIndent :: Int -> Layout ()
setCellIndent i =
  layCells . ix 0 . cellIndent .= i


-- -----------------------------------------------------------------------------
-- Cell Helpers

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
openCell ct = do
  -- Get env info
  fp <- use layFilePath
  r <- use layRegion
  i <- cursorIndent

  -- Build cell and token
  let cell = Cell i ct
      tokLoc = Loc fp r
      cellOpenTok = toCellOpenToken tokLoc cell

  -- Push new cell, insert token
  pushCell cell
  layToks %= Z.insert cellOpenTok


-- | Close a cell, inserting a token at the cursor.
closeCell :: Layout ()
closeCell = do
  -- Fetch some info
  cl <- popCell -- Conviently destroy top cell
  fp <- use layFilePath
  r <- use layRegion

  -- Build and insert a closing token
  let cellCloseTok = toCellCloseToken (Loc fp r) cl
  layToks %= Z.insert cellCloseTok


-- | Close the entire stack of cells at the cursor.
closeCellStack :: Layout ()
closeCellStack = do
  cl <- peekCell
  unless (cl == defCell)
         (closeCell >> closeCellStack)


-- | Convert some cell to an open token
toCellOpenToken:: Loc -> Cell -> Token
toCellOpenToken loc cl =
  case cl ^. cellType of
      Block -> Token TokenBlk "" loc
      LineFold -> Token TokenLn "" loc

-- | Convert some cell to a closed token
toCellCloseToken :: Loc -> Cell -> Token
toCellCloseToken loc cl =
  case cl ^. cellType of
      Block -> Token TokenBlk' "" loc
      LineFold -> Token TokenLn' "" loc
