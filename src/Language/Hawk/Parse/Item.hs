{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
  #-}
module Language.Hawk.Parse.Item where

import Control.Applicative
import Control.Lens
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P
import qualified Text.Megaparsec.Expr       as P


itemP :: MonadParser m
      => ExpOpTable m -> m Item
itemP ops = 
      (ForeignItem <$> foreignP)
  <|> (noconsumeP)
  <|> (DecItem <$> decP)
  <|> (DefItem <$> defP ops)



foreignP :: MonadParser m => m Foreign
foreignP = 
  Foreign <$> (rsvp "foreign" *> foreignTypeP) <*> stringP <*> decP

foreignTypeP :: MonadParser m => m ForeignType
foreignTypeP =
    ForeignC <$ rsvp "ccall"

noconsumeP :: MonadParser m => m Item
noconsumeP =
  NoConsume <$> (rsvp "noconsume" *> varnameP)

decP :: MonadParser m
     => m Dec
decP =
  Dec <$> P.try (varnameP <* rsvp ":") <*> typeP


defP :: MonadParser m
     => ExpOpTable m -> m Def
defP ops =
  Def <$> varnameP <*>  (many patP <* rsvp "=") <*> expP ops




varnameP :: MonadParser m => m Name
varnameP = Name <$> (anyVarId <|> parens anyOpId)

varP :: MonadParser m => m Var
varP = Var <$> anyVarId

opVarP :: MonadParser m => m Var
opVarP = Var <$> parens anyOpId

patP :: MonadParser m => m Pat
patP =
  Pat <$> anyVarId


expP :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
expP ops = dexp ops


dexp :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
dexp ops =
  eifP ops <|> eletP ops <|> cexp ops


cexp :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
cexp ops = P.makeExprParser (bexp ops) ops


bexp :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
bexp ops =
  eapp_ <$> aexp ops <*> many (aexp ops)

aexp :: MonadParser m
      => ExpOpTable m -> m (Exp Var)
aexp ops =
      evarP
  <|> econP
  <|> eprimP 
  <|> elitP 
  <|> parens (expP ops)


evarP :: MonadParser m => m (Exp Var)
evarP = EVar <$> varP

econP :: MonadParser m => m (Exp Var)
econP = ECon <$> conP

elitP :: MonadParser m => m (Exp Var)
elitP = ELit <$> litP

litP :: MonadParser m => m Lit
litP =     
      ( IntLit <$> integerP )
  <|> ( FloatLit <$> doubleP )
  <|> ( CharLit <$> charP )
  


eprimP :: MonadParser m => m (Exp Var)
eprimP =
  EPrim <$> primInstrP


primInstrP :: MonadParser m => m PrimInstr
primInstrP =
          (prim "#add" *> pure PrimAdd)
      <|> (prim "#fadd" *> pure PrimFAdd)
      <|> (prim "#sub" *> pure PrimSub)
      <|> (prim "#fsub" *> pure PrimFSub)
      <|> (prim "#mul" *> pure PrimMul)
      <|> (prim "#fmul" *> pure PrimFMul)
      <|> (prim "#div" *> pure PrimDiv)
      <|> (prim "#udiv" *> pure PrimUDiv)
      <|> (prim "#sdiv" *> pure PrimSDiv)
      <|> (prim "#fdiv" *> pure PrimFDiv)


eifP :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
eifP ops = do
  e1 <- rsvp "if" *> cexp ops
  e2 <- rsvp "then" *> expP ops
  e3 <- rsvp "else" *> expP ops
  return $ EIf e1 e2 e3


eletP :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
eletP ops =
  let_ <$> (rsvp "let" *> block binder)
       <*> (rsvp "in" *> expP ops)
  where
    binder = (,) <$> varP <*> (rsvp "=" *> expP ops)


typeP :: MonadParser m => m Type
typeP = do
    t1 <- atypeP
    (TFun t1 <$> (P.try (rsvp "->") *> typeP))
      <|> return t1

--btypeP :: MonadParser m => m Type
--btypeP = TApp <$> atypeP <*> many atypeP

atypeP :: MonadParser m => m Type
atypeP =
  gconP <|> tvarP <|> parens typeP


gconP :: MonadParser m => m Type
gconP =
      tconP
  <|> tconUnitP


tconP :: MonadParser m => m Type
tconP = TCon <$> conP

tconUnitP :: MonadParser m => m Type
tconUnitP = TCon (Con "Unit") <$ (rsvp "(" *> rsvp ")")

tvarP :: MonadParser m => m Type
tvarP = TVar <$> typevarP

typevarP :: MonadParser m => m TVar
typevarP = TypeVar <$> anyVarId

conP :: MonadParser m => m Con
conP = Con <$> anyConId