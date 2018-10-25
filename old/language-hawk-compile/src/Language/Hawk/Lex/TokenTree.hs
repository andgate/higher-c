{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , LambdaCase
           , OverloadedStrings
           , RankNTypes
           , StandaloneDeriving
  #-}
module Language.Hawk.Lex.TokenTree where

import Control.Lens hiding (Context(..))
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location (Loc(..), Region, locReg, regStart, posLine)



blkTriggers :: [TokenClass]
blkTriggers = [TokenRsvp ":=", TokenRsvp "do", TokenRsvp "where", TokenRsvp "let"]

blkEndTriggers :: [TokenClass]
blkEndTriggers = [TokenRsvp "in"]

-------------------------------------------------------------------------------
-- Token Tree Data Types
-------------------------------------------------------------------------------

-- A token tree represents the tokens of some source file,
-- structured by conforming to whitespace layout rules. 
data TokenTree = T (TNode 'Block)
deriving instance Show TokenTree
deriving instance Eq TokenTree

-- Kinds used to represent the three different structures found in a token tree.
data Group = Line | Block | Item

-- The nodes found in a token tree
data TNode :: Group -> * where
  -- A block is a list of lines at a given indentation level
  TBlock       :: Int -> [TNode 'Line] -> TNode 'Block
  -- A line is a list of items at a given indentation level
  TLine        :: Int -> [TNode 'Item] -> TNode 'Line
  -- An item in the tree contains a token.
  TItem        :: Token -> TNode 'Item
  -- Alternatively, items can sometimes be inline blocks.
  TInlineBlock :: TNode 'Block -> TNode 'Item
deriving instance Show (TNode g)
deriving instance Eq (TNode g)



-------------------------------------------------------------------------------
-- Functional Zipper for Token Tree
-------------------------------------------------------------------------------

-- Possible moves a zipper can take on a token tree.
data Move :: Group -> * where
  -- Root is a top-level block
  RootToBlock    :: Move 'Block
  -- From a block, we can move to a line, leaving behind the lines above and below the selected line.
  BlockToLine    :: Int -> [TNode 'Line] -> [TNode 'Line] -> Move 'Block -> Move 'Line
  -- From a line, we can move to an item, leaving behind the items to the left and to the right of the selected item
  LineToItem     :: Int -> [TNode 'Item] -> [TNode 'Item] -> Move 'Line -> Move 'Item
  -- From an item we might be able to enter an inline block.
  ItemToBlock    :: Move 'Item -> Move 'Block
deriving instance Show (Move g)


data Zipper = forall n. Zipper (TNode n) (Move n)
deriving instance Show Zipper


unZip :: TNode 'Block -> Zipper
unZip = flip Zipper RootToBlock


zipUp :: Zipper -> TNode 'Block
zipUp = \case
  Zipper x RootToBlock            -> x
  Zipper x (BlockToLine i y y' c) -> zipUp $ Zipper (TBlock i (y ++ [x] ++ y')) c
  Zipper x (LineToItem i y y' c)  -> zipUp $ Zipper (TLine i (y ++ [x] ++ y')) c
  Zipper x (ItemToBlock c)        -> zipUp $ Zipper (TInlineBlock x) c


-- Node location that can be zipped too
zipTo :: Zipper -> Zipper
zipTo = \case
  Zipper _ _ -> undefined


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

empty :: TokenTree
empty = T (TBlock 0 [TLine 0 []])


fromList :: [Token] -> TokenTree
fromList = foldr insert empty

insert :: Token -> TokenTree -> TokenTree
insert t tr@(T root) =
  case zipTo (unZip root) of
    Zipper (TLine i xs) c -> insertAt (TItem t) c
    _ -> tr


insertAt :: TNode 'Item -> Move g -> TokenTree
insertAt i c = undefined

{--

tnode :: Token -> TokenTree 'Item
tnode t = TokenNode t

bnode :: Token -> TokenTree 'Item

bnode t = BlockNode (TokenBlock i [TokenLine i [TokenNode t]])
  where i = t^.tokLoc.locReg.regStart.posLine


emptyBlock :: TokenTree 'Block
emptyBlock = TokenBlock 0 [emptyLine]

emptyLine :: TokenTree 'Line
emptyLine = TokenLine 0 []


fromList :: [Token] -> TokenTree 'Block
fromList = foldr insert emptyBlock



insert :: Token -> TokenTree 'Block -> TokenTree 'Block
insert t (TokenBlock i lns)
  | ti >= i   = case lns of
                  [] -> TokenBlock i [insertLine t emptyLine]
                  ln:lns' -> TokenBlock i (insertLine t ln : lns')
                  
  | otherwise = TokenBlock i lns
  where
    ti = t^.tokLoc.locReg.regStart.posLine


insertLine :: Token -> TokenTree 'Line -> TokenTree 'Line
insertLine t (TokenLine i ns)
  | ti > i    = TokenLine i (insertNode t ns)
  | otherwise = TokenLine i ns
  where
    ti = t^.tokLoc.locReg.regStart.posLine


insertNode :: Token -> [LineNode] -> [LineNode]
insertNode t1 = \case
  ns | blkTrig t1 -> bnode t1 : ns
      
  [] -> [tnode t1]

  (TokenNode t2):ns | blkEndTrig t2 ->
    tnode t2 : ns

  (TokenNode t2):ns ->
    tnode t1 : tnode t2 : ns

  (BlockNode b):ns ->
    BlockNode (insert t1 b) : ns 

  where
    blkTrig t = t^.tokClass `elem` blkTriggers
    blkEndTrig t = t^.tokClass `elem` blkEndTriggers
--}
