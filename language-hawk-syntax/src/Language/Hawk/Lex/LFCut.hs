module Language.Hawk.Lex.LFCut where

import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location

-- | Split a list of tokens at toplevel linefolds
lfCut :: [Token] -> [[Token]]
lfCut = lfCut' [] []

lfCut' :: [Token] -> [[Token]] -> [Token] -> [[Token]]
lfCut' cur cut [] = tail $ reverse $ (reverse cur):cut
lfCut' cur cut (t@(Token TokenLn _ (Loc _ (R (P _ 0) _))):ts) = 
  lfCut' [t] ((reverse cur):cut) ts
lfCut' cur cut ((Token TokenEof _ _):ts) = 
  lfCut' cur cut ts
lfCut' cur cut (t:ts) = 
  lfCut' (t:cur) cut ts