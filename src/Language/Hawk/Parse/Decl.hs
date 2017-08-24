{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
  #-}
module Language.Hawk.Parse.Decl where

import Control.Applicative
import Control.Lens
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L



{- 

Concrete syntax for the language: 
Optional components in this BNF are marked with < >

  terms:
    a,b,A,B ::=
      *                        Intuitionistic Universe
    | o                        Linear Universe
    | x                        Variables   (start with lowercase)
    | \ x . a                  Function definition
    | a b                      Application
    | (x : A) -> B             Pi type
    | (x : A) -o B             Linear Pi Type

    | (a : A)                  Annotations
    | (a)                      Parens
    | _                        An axiom hole, inhabits all types

    | let x = a in b           Let expression
    | if a then b else c       If 

    | Type Literal             Type level Literal
    | Literal                  Value level Literal
    | Prim                     Primitive instructions

    | dup a                    Duplicates a variable without consuming
    | free a in b              Free a variable and evaluates b

    | Bool                     Boolean type
    | True | False             Boolean values



  declarations:

      foo : A
      foo = a


  Syntax sugar:

   - You can collapse lambdas, like:

         \ x [y] z . a

     This gets parsed as \ x . \ [y] . \ z . a

-}

declP :: MonadParser m
      => m Decl
declP = 
      sigP
  <|> defP
  <|> recdefP
  <|> foreignP
  <|> fixityP



fixityP :: MonadParser m =>  m Decl
fixityP = do
  fx <- try infixP
  p <- fromIntegral <$> integerP
  ops <- some anyOpId

  return $ Fixity fx p ops
    

infixP :: MonadParser m => m Fixity
infixP =
  infixDecN <|> infixDecL <|> infixDecR


infixP :: MonadParser m => m Fixity
infixP = rsvp "infix" *> pure InfixN

infixlP :: MonadParser m => m Fixity
infixlP = rsvp "infixl" *> pure InfixL

infixrP :: MonadParser m => m Fixity
infixrP = rsvp "infixr" *> pure InfixR




foreignP :: MonadParser m => m Foreign
foreignP = 
  Foreign <$> (rsvp "foreign" *> foreignTypeP) <*> stringP <*> decP

foreignTypeP :: MonadParser m => m ForeignType
foreignTypeP =
    ForeignC <$ rsvp "ccall"


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
expP ops =
  dexp ops


dexp :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
dexp ops =
  eifP ops <|> eletP ops <|> cexp ops


cexp :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
cexp ops =
  P.makeExprParser (bexp ops) ops


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
evarP = elocP $ EVar <$> varP


econP :: MonadParser m => m (Exp Var)
econP = elocP $ ECon <$> conP


elitP :: MonadParser m => m (Exp Var)
elitP = elocP $ ELit <$> litP


litP :: MonadParser m => m Lit
litP =     
      ( IntLit <$> integerP )
  <|> ( FloatLit <$> doubleP )
  <|> ( CharLit <$> charP )
  


eprimP :: MonadParser m => m (Exp Var)
eprimP = elocP $
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
eifP ops = elocP $ do
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
    (TApp t1 <$> (P.try (rsvp "->") *> typeP))
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
tconP = TCon <$> tyconP

tconUnitP :: MonadParser m => m Type
tconUnitP = tUnit <$ (rsvp "(" *> rsvp ")")

tvarP :: MonadParser m => m Type
tvarP = TVar <$> typevarP

typevarP :: MonadParser m => m Tyvar
typevarP = Tyvar <$> anyVarId

conP :: MonadParser m => m Con
conP = Con <$> anyConId

tyconP :: MonadParser m => m Tycon
tyconP = Tycon <$> anyConId



dataDeclP :: MonadParser m => m DataDecl
dataDeclP =
  DataDecl <$> (rsvp "data" *> anyConName) <*> (rsvp "=" *> some conDecl)


conDecl :: MonadParser m => m ConDecl
conDecl =
      (ConDecl <$> anyConName <*> many atypeP)
  <|> (RecDecl <$> anyConName <*> curlyBrackets (commaSep1 recField))

recField :: MonadParser m => m RecField
recField =
  RecField <$> anyVarName <*> (rsvp ":" *> atypeP)