{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , TemplateHaskell
  #-}
module Language.Hawk.Parse.Lexer.Layout where

import Control.Lens
import Control.Monad (when, unless, void)
import Control.Monad.State.Strict (State, evalState)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.SrcLoc (SrcLoc(..))
import Language.Hawk.Report.Region (Region)

import qualified Language.Hawk.Report.Region  as R


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
    , _layIn :: [Token]
    , _layOut :: [Token]
    } deriving (Eq, Ord, Show)

makeLenses ''LayoutState

type Layout = State LayoutState

mkLayout :: [Token] -> LayoutState
mkLayout input = LayoutState  "" mempty [defCell] False input []


-- -----------------------------------------------------------------------------
-- Layout Driver


-- This would be more performant with a foldM in the State monad
-- Since this was originally implemented with conduit, using recursion in a state monad that
-- maintains input/out lists was easier.
layout :: [Token] -> [Token]
layout =
  evalState layoutDriver . mkLayout

layoutDriver :: Layout [Token]
layoutDriver = do
  ts <- use layIn
  case ts of
    (t:ts') -> do layIn .= ts'
                  updateLocation t
                  handleTok t
                  layoutDriver
    [] -> reverse <$> use layOut

handleTok :: Token -> Layout ()
handleTok t
  | t^.tokClass == TokenEof = closeStack

  | otherwise = do
      -- Blocks triggered on last token need to be handled
      emitBlk <- use blkTriggered
      when emitBlk $ do
        blkTriggered .= False
        open Block

      closeInvalid
      yieldTok t

      -- Colons trigger block emission for the next token
      when (t^.tokClass == TokenRsvp ":")
           (blkTriggered .= True)


-- -----------------------------------------------------------------------------
-- Driver Helpers
  

yieldTok :: Token -> Layout ()
yieldTok t =
  layOut %= (t:)

closeStack :: Layout ()
closeStack = do
  cl <- peekCell
  unless (cl == defCell)
         (close >> closeStack)


closeInvalid :: Layout ()
closeInvalid = do
  go =<< getCurrIndent
  fillBlock
  where
    go i = do
      cl <- peekCell
      unless (isValid i cl)
             (close >> go i)


fillBlock :: Layout ()
fillBlock = do
  (Cell _ ct) <- peekCell
  when (ct == Block)
       (open LineFold)
  
      
open :: CellType -> Layout ()
open ct = do
  fp <- use layFilePath
  r <- use layRegion
  i <- getCurrIndent

  let cl = Cell i ct
  pushCell cl
  yieldTok $ openTok (SrcLoc fp r) cl

close :: Layout ()
close = do
  cl <- peekCell
  fp <- use layFilePath
  r <- use layRegion
  yieldTok $ closeTok (SrcLoc fp r) cl
  void popCell


-- -----------------------------------------------------------------------------
-- Layout Helpers

updateLocation :: Token -> Layout ()
updateLocation (Token _ _ (SrcLoc fp r)) = do
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

    
openTok :: SrcLoc -> Cell -> Token
openTok loc cl =
  case cl ^. cellType of
      Block -> Token TokenBlk "" loc
      LineFold -> Token TokenLn "" loc


closeTok :: SrcLoc -> Cell -> Token
closeTok loc cl =
  case cl ^. cellType of
      Block -> Token TokenBlk' "" loc
      LineFold -> Token TokenLn' "" loc
  
  
isValid :: Int -> Cell -> Bool
isValid i (Cell ci ct) =
  case ct of
    Block -> 
      ci <= i
    
    LineFold ->
      ci < i