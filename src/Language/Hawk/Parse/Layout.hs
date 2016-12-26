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



-- -----------------------------------------------------------------------------
-- Layout Environment

type LayoutPosition = (Int64, Int64)

data LayoutEnv =
  LayoutEnv {
    layoutPos  :: [LayoutPosition],
    lastComment :: String
  }
  deriving (Show)


defLayoutEnv :: LayoutEnv
defLayoutEnv = LayoutEnv [] ""
  
class HasLayoutEnv u where
    layoutEnv :: Simple Lens u LayoutEnv

instance HasLayoutEnv LayoutEnv where
    layoutEnv = id




showIndent :: LayoutPosition -> String
showIndent (_,c) = case c of
    0 -> "top-level indentation"
    x -> show x ++ "-column indentation"


isIndentValid :: Int64 -> Int64 -> Bool
isIndentValid = (==)

isIndentInvalid :: Int64 -> Int64 -> Bool
isIndentInvalid = (/=)
  

-- -----------------------------------------------------------------------------
-- Layout State

type LayoutState m =
  MonadState LayoutEnv m


-- Layout Column
pushPos :: LayoutState m => LayoutPosition -> m ()
pushPos pos = do
  LayoutEnv poss com <- get
  put (LayoutEnv (pos:poss) com)

popPos :: LayoutState m => m LayoutPosition
popPos = do
  pos <- peekPos
  LayoutEnv poss com <- get
  let poss' = tail poss
      res = LayoutEnv poss' com
  put res
  return pos
  
peekPos :: LayoutState m => m LayoutPosition
peekPos = do
  LayoutEnv poss _ <- get
  return $ headDef (0,0) poss


-- Last Comment
putComment :: LayoutState m => String -> m ()
putComment com = do
  LayoutEnv poss _ <- get
  put $ LayoutEnv poss com
  
getComment :: LayoutState m => m String
getComment = do
  LayoutEnv _ com <- get
  return com


class (LayoutState m, DeltaParsing m, LookAheadParsing m) => LayoutParsing m where
    ws :: m ()
    ws = padding



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
  layPos <- peekPos
  curPos <- getCurPos
  return (layPos, curPos)

getLayAndCurCol :: LayoutParsing m => m (Int64, Int64)
getLayAndCurCol = do
  (_, layCol) <- peekPos
  curCol <- getCurCol
  return (layCol, curCol)
  
getLayAndCurRow :: LayoutParsing m => m (Int64, Int64)
getLayAndCurRow = do
  (layRow, _) <- peekPos
  curRow <- getCurRow
  return (layRow, curRow)


-- -----------------------------------------------------------------------------
-- Padding Combinators


padding :: LayoutParsing m => m ()
padding = do
  optional (try spaces)
  processComments <?> "padding"


pad :: LayoutParsing m => m a -> m a
pad p =
  ws *> p <* ws


lpad :: LayoutParsing m => m a -> m a
lpad p =
  ws *> p

rpad :: LayoutParsing m => m a -> m a
rpad p =
  p <* ws


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
  (_, layCol) <- peekPos
  curPos@(_, curCol) <- getCurPos
  when (curCol <= layCol)
    $ error (showIndent curPos)
    
    
same :: LayoutParsing m => m ()
same = do 
  (layRow, _) <- peekPos
  curRow <- getCurRow
  when (curRow /= layRow)
    $ error "line break"


block :: LayoutParsing m => m a -> m [a]
block p = withPos $ some (ws >> checkIndent >> p)


withPos :: LayoutParsing m => m a -> m a
withPos p = 
  getCurPos >>= pushPos >> p


checkIndent :: LayoutParsing m => m ()
checkIndent = do
  layPos@(_, layCol) <- peekPos
  curPos@(_, curCol) <- getCurPos
  when (layCol /= curCol) $
        (<?> showIndent layPos)
        (fail $ showIndent curPos)



-- Thinking about how this parser should more...
-- Reading the docs for Text.Parsec.Indent has made me realize that
-- the layout api should be the primary backbone of the syntax parser.
--   withBlock
--   sameOrIndented
--   paddedSame
--   paddedSameOrIndented


-- -----------------------------------------------------------------------------
-- Comments

processComments :: LayoutParsing m => m ()
processComments = do
  cs <- comments
  putComment $ lastDef "" cs

comments :: LayoutParsing m => m [String]
comments =
  many comment <?> "comments"

comment :: LayoutParsing m => m String
comment =
        (singleLineComment <?> "Single-Line Comment")
    <|> (multiLineComment <?> "Multi-Line Comment")

singleLineComment :: LayoutParsing m => m String
singleLineComment = try $ do
  spaces
  string "//"
  c <- manyTill anyChar newline
  spaces
  return c

multiLineComment :: LayoutParsing m => m String
multiLineComment = try $ do
  string "/*"
  spaces
  c <- manyTill anyChar (string "*/")
  spaces
  return c
  
  
notComment :: LayoutParsing m => m String
notComment = 
  manyTill anyChar $ lookAhead (void comment <|> eof <|> spaces) 