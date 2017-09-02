{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
  #-}
module Language.Hawk.Parse.Term where

import Control.Applicative
import Control.Lens
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text, pack)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L

import Unbound.Generics.LocallyNameless


expP :: MonadParser m
     => m () -> m Exp
expP = cexp


cexp :: MonadParser m
  => m () -> m Exp
cexp ws =
  P.choice [ ifP ws
           , letP ws
           , bexp ws
           ]


bexp :: MonadParser m
  => m () -> m Exp
bexp ws =
  P.try (EApp <$> aexp ws <*> bexp ws)
  <|> aexp ws


aexp :: MonadParser m
     => m () -> m Exp
aexp ws =
  P.choice [ varP ws
           , conP ws
           , primP ws
           , litP ws
           , parenP ws
           ]


varP :: MonadParser m => m () -> m Exp
varP ws = elocP $ EVar <$> varName ws


conP :: MonadParser m => m () -> m Exp
conP ws = elocP $ ECon <$> conName ws


litP :: MonadParser m => m () -> m Exp
litP ws = elocP $ ELit <$> litP' ws

litP' :: MonadParser m => m () -> m Lit
litP' ws =
  P.choice [ IntLit <$> integerP ws
           , FloatLit <$> doubleP ws
           , CharLit <$> charP ws
           ]

primP :: MonadParser m => m () -> m Exp
primP ws = elocP $
  EPrim <$> primInstrP ws


primInstrP :: MonadParser m => m () -> m PrimInstr
primInstrP ws =
  P.choice
    [ prim "#add" *> pure PrimAdd
    , prim "#fadd" *> pure PrimFAdd
    , prim "#sub" *> pure PrimSub
    , prim "#fsub" *> pure PrimFSub
    , prim "#mul" *> pure PrimMul
    , prim "#fmul" *> pure PrimFMul
    , prim "#div" *> pure PrimDiv
    , prim "#udiv" *> pure PrimUDiv
    , prim "#sdiv" *> pure PrimSDiv
    , prim "#fdiv" *> pure PrimFDiv
    ]
  where
    prim = L.lexeme ws . P.string

parenP :: MonadParser m => m () -> m Exp
parenP ws =
  EParen <$> parens ws (expP ws)


ifP :: MonadParser m
     => m () -> m Exp
ifP ws = elocP $ do
  P.try $ rsvp ws "if"
  a <- bexp ws
  rsvp ws "then"
  b <- expP ws
  rsvp ws "else"
  c <- expP ws
  return $ EIf a b c


letP :: MonadParser m
     => m () -> m Exp
letP ws = do
  rsvp ws "let"
  x <- varName ws
  equals ws
  a <- expP ws
  rsvp ws "in"
  b <- expP ws
  return $ ELet (x, a) b 


lambdaP :: MonadParser m => m () -> m Exp
lambdaP ws = do
  backslash ws
  binds <- P.some (varName ws)
  dot ws
  body <- expP ws
  return $ foldr ELam body binds


typeP :: MonadParser m => m () -> m Type
typeP ws = do
    t1 <- atypeP ws
    (TApp t1 <$> (P.try (arrowr ws) *> typeP ws))
      <|> return t1

btypeP :: MonadParser m => m () -> m Type
btypeP ws = (TApp <$> btypeP ws <*> atypeP ws)
            <|> atypeP ws

atypeP :: MonadParser m => m () -> m Type
atypeP ws =
  gconP ws <|> tvarP ws <|> parens ws (typeP ws)


gconP :: MonadParser m => m() -> m Type
gconP ws =
      tconP ws
  <|> tconUnitP ws


tconP :: MonadParser m => m () -> m Type
tconP ws = TCon <$> conName ws

tconUnitP :: MonadParser m => m () -> m Type
tconUnitP ws =
  tUnit <$ (L.symbol ws "()")

tvarP :: MonadParser m => m () -> m Type
tvarP ws = TVar <$> varName ws


dataDeclP :: MonadParser m => m () -> m DataDecl
dataDeclP ws =
  DataDecl <$> (rsvp ws "data" *> conName ws) <*> (equals ws *> some (conDecl ws))


conDecl :: MonadParser m => m () -> m ConDecl
conDecl ws =
      (ConDecl <$> conName ws <*> many (atypeP ws))
  <|> (RecDecl <$> conName ws <*> braces ws (commaSep1 ws (recField ws)))

recField :: MonadParser m => m () -> m RecField
recField ws =
  RecField <$> varName ws <*> (colon ws *> atypeP ws)
