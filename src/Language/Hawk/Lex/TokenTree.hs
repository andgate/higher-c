{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , TemplateHaskell
           , DataKinds
           , KindSignatures
           , GADTs
           , LambdaCase
  #-}
module Language.Hawk.Lex.TokenTree where


import Control.Lens
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location (Loc(..), Region, locReg, regStart, posLine)


data Group = Line | Block


-- Blocks can only contain lines, but can be on a line
data TokenTree :: Group -> * where
  TokenLine      :: Int -> [LineNode] -> TokenTree 'Line
  TokenBlock     :: Int -> [TokenTree 'Line] -> TokenTree 'Block

data LineNode
  = TokenNode Token
  | BlockNode (TokenTree 'Block)

tnode :: Token -> LineNode
tnode t = TokenNode t

bnode :: Int -> LineNode
bnode i = BlockNode (TokenBlock i [TokenLine i []])


empty :: TokenTree 'Block
empty = TokenBlock 0 [TokenLine 0 []]


fromList :: [Token] -> TokenTree 'Block
fromList = foldr insert empty


insert :: Token -> TokenTree a -> TokenTree 'Block
insert t = \case
  TokenLine  i ns -> empty
  TokenBlock i ls -> empty
  where ti = t^.tokLoc.locReg.regStart.posLine
