{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Parse.Module where


import Control.Lens
import Data.Text (Text)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax


import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Pos        as P



moduleP :: FilePath -> ParserT () Identity ModPs
moduleP fp = do
  rsvp "mod"
  ns <- modPathP
  ts <- P.many anyTok
  return $ mkModPs ns fp ts


modPathP :: ParserT () Identity [Text]
modPathP = undefined