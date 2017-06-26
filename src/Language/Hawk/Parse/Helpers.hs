{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Parse.Helpers where

import Control.Lens
import Data.Text (Text, unpack)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax.Location

import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Pos        as P


type ParserT u m a = P.ParsecT [Token] u m a


toSrcPos :: (HasLocation l, HasRegion l) => l -> P.SourcePos
toSrcPos l =
    P.newPos  (l^.locPath)
              (l^.regStart^.posLine)
              (l^.regStart^.posColumn)


tokP :: Monad m => TokenClass -> ParserT u m Token
tokP x
   = P.tokenPrim showTok nextPos testTok
   where
     showTok t        = "'" ++ (unpack $ t^.tokText) ++ "'"
     nextPos p1 t ts  = toSrcPos (head ts)
     testTok t        = if x == t^.tokClass then Just t else Nothing


anyTok :: Monad m => ParserT u m Token
anyTok
   = P.tokenPrim showTok nextPos testTok
   where
     showTok t        = "'" ++ (unpack $ t^.tokText) ++ "'"
     nextPos p1 t ts  = toSrcPos (head ts)
     testTok t        = Just t


-- -----------------------------------------------------------------------------
-- Terminal Production Helpers

rsvp :: Monad m => Text -> ParserT u m Token
rsvp
  = tokP . TokenRsvp 


prim :: Monad m => Text -> ParserT u m Token
prim
  = tokP . TokenPrim 


op :: Monad m => Text -> ParserT u m Token
op
  = tokP . TokenPrim


-- -----------------------------------------------------------------------------
-- Combinator Helpers

parens :: Monad m => ParserT u m a -> ParserT u m a
parens p
  = rsvp "(" *> p <* rsvp ")"

sqrBrackets :: Monad m => ParserT u m a -> ParserT u m a
sqrBrackets p
  = rsvp "[" *> p <* rsvp "]"

curlyBrackets :: Monad m => ParserT u m a -> ParserT u m a
curlyBrackets p
  = rsvp "{" *> p <* rsvp "}"

angleBrackets :: Monad m => ParserT u m a -> ParserT u m a
angleBrackets p
  = rsvp "<" *> p <* rsvp ">"


-- -----------------------------------------------------------------------------
-- Layout Helpers

block :: Monad m => ParserT u m a -> ParserT u m [a]
block p = blk *> linefolds p <* blk'

linefolds0 :: Monad m => ParserT u m a -> ParserT u m [a]
linefolds0 p = P.many $ linefold p

linefolds :: Monad m => ParserT u m a -> ParserT u m [a]
linefolds p = P.many1 $ linefold p

linefold :: Monad m => ParserT u m a -> ParserT u m a
linefold p = ln *> p <* ln'


blk :: Monad m => ParserT u m Token
blk = tokP TokenBlk

blk' :: Monad m => ParserT u m Token
blk' = tokP TokenBlk'


ln :: Monad m => ParserT u m Token
ln = tokP TokenLn

ln' :: Monad m => ParserT u m Token
ln' = tokP TokenLn'


eof :: Monad m => ParserT u m Token
eof = tokP  TokenEof