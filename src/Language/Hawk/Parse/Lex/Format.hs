{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , TemplateHaskell
  #-}
module Language.Hawk.Parse.Lex.Format where

import Control.Lens
import Control.Monad (when, unless, void)
import Control.Monad.State.Strict (State, evalState)
import Safe (headDef)
import Language.Hawk.Parse.Token
import Language.Hawk.Syntax.Location (Loc(..), Region)

import qualified Language.Hawk.Syntax.Location  as L


blkTriggers :: [TokenClass]
blkTriggers = [TokenRsvp ":=", TokenRsvp "do", TokenRsvp "where", TokenRsvp "let"]

blkEndTriggers :: [TokenClass]
blkEndTriggers = [TokenRsvp "in"]

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
    , _layToks :: [Token]
    , _layToks' :: [Token]
    , _layResults :: [[Token]]
    } deriving (Eq, Ord, Show)

makeLenses ''LayoutState

type Layout = State LayoutState

mkLayout :: [Token] -> LayoutState
mkLayout input = LayoutState  "" mempty [defCell] False input [] []


-- -----------------------------------------------------------------------------
-- Layout Driver


-- This would be more performant with a foldM in the State monad
-- Since this was originally implemented with conduit, using recursion in a state monad that
-- maintains input/out lists was easier.
layout :: [Token] -> [[Token]]
layout =
  evalState layoutDriver . mkLayout

layoutDriver :: Layout [[Token]]
layoutDriver = do
  ts <- use layToks
  case ts of
    (t:ts') -> do layToks .= ts'
                  updateLocation t
                  handleTok t
                  layoutDriver
    [] -> reverse <$> use layResults

handleTok :: Token -> Layout ()
handleTok t
  | t^.tokClass == TokenEof = closeStack

  | otherwise = do
      -- Blocks triggered on last token need to be handled
      emitBlk <- use blkTriggered
      when emitBlk $ do
        blkTriggered .= False
        open Block
      
      when (t^.tokClass `elem` blkEndTriggers)
           closeBlk

      closeInvalid
      yieldTok t

      -- Colons trigger block emission for the next token
      when (t^.tokClass `elem` blkTriggers)
           (blkTriggered .= True)


-- -----------------------------------------------------------------------------
-- Driver Helpers
  

yieldTok :: Token -> Layout ()
yieldTok t = do
  layToks' %= (t:)
  
  when ( (t ^. L.regStart . L.posColumn) == 0
         && (t ^. tokClass == TokenLn')
       )
       $ do ts <- use layToks'
            layResults %= (reverse ts:)
            return ()


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
  yieldTok $ openTok (Loc fp r) cl

close :: Layout ()
close = do
  cl <- peekCell
  fp <- use layFilePath
  r <- use layRegion
  yieldTok $ closeTok (Loc fp r) cl
  void popCell


-- | This will close a block layout, if there is one.
-- | Otherwise, it will just close a linefold, if there is one.
closeBlk :: Layout ()
closeBlk = do
  -- Peek two cells off the stack
  (Cell i1 ct1) <- peekCell
  (Cell i2 ct2) <- uses layStack (headDef defCell . tail)

  -- Close twice when there is a linefold followed by a block that isn't root
  when (ct1 == LineFold && ct2 == Block && i2 /= 0)
       (close >> close)

  -- Close once when there is a block that isn't root
  when (ct1 == Block && i1 /= 0)
       (close)


-- -----------------------------------------------------------------------------
-- Layout Helpers

updateLocation :: Token -> Layout ()
updateLocation (Token _ _ (Loc fp r)) = do
    layFilePath .= fp
    layRegion .= r

getCellIndent :: Layout Int
getCellIndent =
  _cellIndent <$> peekCell

getCurrIndent :: Layout Int
getCurrIndent =
  use $ layRegion . L.regStart . L.posColumn
    
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
  uses layStack (headDef defCell)

    
openTok :: Loc -> Cell -> Token
openTok loc cl =
  case cl ^. cellType of
      Block -> Token TokenBlk "" loc
      LineFold -> Token TokenLn "" loc


closeTok :: Loc -> Cell -> Token
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
