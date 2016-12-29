{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Hawk.Parse.Layout where

import Control.Applicative
import Control.Arrow
import Control.Lens (Simple, Lens, over, (^.))
import Control.Monad.State
import Data.Int
import Safe
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R



-- -----------------------------------------------------------------------------
-- Layout Environment

type LayoutPosition = (Int64, Int64)

data Layout =
    LineLayout LayoutPosition
  | BlockLayout LayoutPosition
  deriving (Show)

data LayoutEnv =
  LayoutEnv {
    layoutStack  :: [Layout],
    commentCache :: Maybe String -- Holds the last comment from the previous chunk of whitespace.
  }
  deriving (Show)


layoutPosition :: Layout -> LayoutPosition
layoutPosition lay =
  case lay of
    LineLayout p -> p
    BlockLayout p -> p

rootLayout :: Layout
rootLayout = BlockLayout (0,0)

defLayoutEnv :: LayoutEnv
defLayoutEnv = LayoutEnv [rootLayout] Nothing
  
class HasLayoutEnv u where
    layoutEnv :: Simple Lens u LayoutEnv

instance HasLayoutEnv LayoutEnv where
    layoutEnv = id

showIndent :: LayoutPosition -> String
showIndent (r,c) = 
  show r ++ ":" ++ show c
  

-- -----------------------------------------------------------------------------
-- Layout State

type LayoutState m =
  MonadState LayoutEnv m


-- Layout Column
pushLineLay :: LayoutState m => LayoutPosition -> m ()
pushLineLay = do
  pushLay . LineLayout
  
  
pushBlockLay :: LayoutState m => LayoutPosition -> m ()
pushBlockLay = 
  pushLay . BlockLayout

pushLay :: LayoutState m => Layout -> m ()
pushLay lay = do
  LayoutEnv ls com <- get
  put (LayoutEnv (lay:ls) com)

popLay :: LayoutState m => m Layout
popLay = do
  lay <- peekLay
  LayoutEnv ls com <- get
  let ls' = tailDef [] ls
      res = LayoutEnv ls' com
  put res
  return lay
  
peekLay :: LayoutState m => m Layout
peekLay = do
  (headDef rootLayout . layoutStack) <$> get
  
peekLayPos :: LayoutState m => m LayoutPosition
peekLayPos = do
  (layoutPosition . headDef rootLayout . layoutStack) <$> get


-- Comment Cache
putComment :: LayoutState m => Maybe String -> m ()
putComment str = do
  LayoutEnv ls _ <- get
  put $ LayoutEnv ls str
  
getComment :: LayoutState m => m (Maybe String)
getComment =
  commentCache <$> get


class (LayoutState m, DeltaParsing m, LookAheadParsing m) => LayoutParsing m where
    ws :: m ()
    ws = spaces >> processComments >> checkLayout



instance (DeltaParsing m, LookAheadParsing m) => LayoutParsing (StateT LayoutEnv m)



getCurPos :: LayoutParsing m => m LayoutPosition
getCurPos =
  (,) <$> getCurRow <*> getCurCol

getCurCol :: LayoutParsing m => m Int64
getCurCol =
  column <$> position
  
getCurRow :: LayoutParsing m => m Int64
getCurRow =
  position >>= getCurRow'
  
getCurRow' :: LayoutParsing m => Delta -> m Int64
getCurRow' dt = case dt of
  (Columns _ _) -> error "Layout cannot get current row from Columns Delta."
  (Tab _ _ _) -> error "Layout cannot get current row from Columns Tab."
  (Lines lNum _ _ _) -> return lNum
  (Directed _ lNum _ _ _) -> return lNum


getLayAndCurPos :: LayoutParsing m => m (LayoutPosition, LayoutPosition)
getLayAndCurPos = do
  layPos <- peekLayPos
  curPos <- getCurPos
  return (layPos, curPos)

getLayAndCurCol :: LayoutParsing m => m (Int64, Int64)
getLayAndCurCol = do
  (_, layCol) <- peekLayPos
  curCol <- getCurCol
  return (layCol, curCol)
  
getLayAndCurRow :: LayoutParsing m => m (Int64, Int64)
getLayAndCurRow = do
  (layRow, _) <- peekLayPos
  curRow <- getCurRow
  return (layRow, curRow)


processIndentation :: LayoutParsing m => m ()
processIndentation = do
  (layRow, layCol) <- peekLayPos
  (curRow, curCol) <- getCurPos
  when (curCol < layCol && curRow > layRow)
       (popLay >> processIndentation)

-- -----------------------------------------------------------------------------
-- Comments


commented :: LayoutParsing m => m a -> m (A.Commented a)
commented p = do
  comm <- getComment
  p1 <- position
  r <- p
  p2 <- position
  return $ A.A ((R.mkRegion p1 p2), comm) r

-- This needs to be abusable
processComments :: LayoutParsing m => m ()
processComments = do
  cs <- comments
  putComment $ lastMay cs

comments :: LayoutParsing m => m [String]
comments =
  many comment

comment :: LayoutParsing m => m String
comment =
      (try singleLineComment <?> "Single-Line Comment")
  <|> (multiLineComment <?> "Multi-Line Comment")

singleLineComment :: LayoutParsing m => m String
singleLineComment = do
  try spaces >> string "//"
  c <- manyTill anyChar newline
  try spaces
  return c

multiLineComment :: LayoutParsing m => m String
multiLineComment = do
  try $ spaces >> string "/*"
  c <- manyTill anyChar (string "*/")
  spaces
  return c <?> "Multi-Line Comment"
  
  
notComment :: LayoutParsing m => m String
notComment = 
  manyTill anyChar $ lookAhead (void comment <|> eof <|> spaces) 


-- -----------------------------------------------------------------------------
-- Layout combinators


topLevel :: LayoutParsing m => m ()
topLevel = do
  (_, curCol) <- getCurPos
  unless (curCol == 0) $ unexpected "indentation"
  
notTopLevel :: LayoutParsing m => m ()
notTopLevel = do
  (_, curCol) <- getCurPos
  when (curCol == 0) $ unexpected "top-level"

indented :: LayoutParsing m => m ()
indented = do 
  (_, layCol) <- peekLayPos
  curPos@(_, curCol) <- getCurPos
  when (curCol <= layCol)
    $ unexpected ("not indented. " ++ showIndent curPos)
    
    
same :: LayoutParsing m => m ()
same = do 
  (layRow, _) <- peekLayPos
  curRow <- getCurRow
  when (curRow /= layRow)
    $ unexpected "line break"

sameOrIndented :: LayoutParsing m => m ()
sameOrIndented = do
  layPos@(layRow, layCol) <- peekLayPos
  curPos@(curRow, curCol) <- getCurPos
  when (curCol <= layCol || curRow /= layRow) $
        unexpected (showIndent curPos ++ ":Layout not same line or indented @ " ++ showIndent layPos)



blockLayout :: LayoutParsing m => m a -> m [a]
blockLayout p = 
 enterBlock *> some p <* exitBlockLayout

lineLayout :: LayoutParsing m => m a -> m a
lineLayout p = 
 enterLine *> p <* exitLineLayout


enterBlock :: LayoutParsing m => m ()
enterBlock = 
  getCurPos >>= pushBlockLay

enterLine :: LayoutParsing m => m ()
enterLine = 
  getCurPos >>= pushLineLay
  
exitLayout :: LayoutParsing m => m ()
exitLayout = do
  lay <- peekLay
  case lay of
    BlockLayout _ -> checkBlockLayout
    LineLayout _ -> checkLineLayout


exitBlockLayout :: LayoutParsing m => m ()
exitBlockLayout = do
  (layCol, curCol) <- getLayAndCurCol
  when (layCol > curCol)
       (fail $ "Cannot exit block")
  void popLay

    
exitLineLayout :: LayoutParsing m => m ()
exitLineLayout = do
  (layCol, curCol) <- getLayAndCurCol
  when (layCol >= curCol)
       (fail $ "Cannot exit line")
  void popLay


checkLayout :: LayoutParsing m => m ()
checkLayout = do
  lay <- peekLay
  case lay of
    BlockLayout _ -> checkBlockLayout
    LineLayout _ -> checkLineLayout
    
    
checkBlockLayout :: LayoutParsing m => m ()
checkBlockLayout = do
  (layCol, curCol) <- getLayAndCurCol
  when (layCol > curCol)
       (fail $ "Block layout invalidated")
  void popLay

    
checkLineLayout :: LayoutParsing m => m ()
checkLineLayout = do
  (layCol, curCol) <- getLayAndCurCol
  when (layCol >= curCol)
       (fail $ "Line layout invalidated")
  void popLay



-- Thinking about how this parser should more...
-- Reading the docs for Text.Parsec.Indent has made me realize that
-- the layout api should be the primary backbone of the syntax parser.
--   withBlock
--   sameOrIndented
--   paddedSame
--   paddedSameOrIndented