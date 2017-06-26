{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Parse.Module where


import Control.Lens
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token


import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Pos        as P


data GModule i
  = Module
      { _modlName :: String
      , _modlItems :: [i]
      }


modulep :: ParserT () Identity [Token]
modulep = do
  rsvp "mod" 
  P.many1 anyTok
