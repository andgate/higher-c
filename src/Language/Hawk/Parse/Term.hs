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

termP :: MonadParser m
     => m () -> m Term
termP = cterm


cterm :: MonadParser m
     => m () -> m Term
cterm ws =
  P.choice [ ifP ws
           , letP ws
           , bterm ws
           ]


bterm :: MonadParser m
     => m () -> m Term
bterm ws =
  P.try (App <$> aterm ws <*> bterm ws)
  <|> aterm ws


aterm :: MonadParser m
      => m () -> m Term
aterm ws =
  P.choice [ varP ws
           , conP ws
           , primP ws
           , litP ws
           , parenP ws
           ]


varP :: MonadParser m => m () -> m Term
varP ws = tlocP $ Var . s2n <$> varid ws


conP :: MonadParser m => m () -> m Term
conP ws = tlocP $ Con . pack <$> conid ws


litP :: MonadParser m => m () -> m Term
litP ws = tlocP $ Lit <$> litP' ws

litP' :: MonadParser m => m () -> m Lit
litP' ws =
  P.choice [ IntLit <$> integerP ws
           , FloatLit <$> doubleP ws
           , CharLit <$> charP ws
           ]

primP :: MonadParser m => m () -> m Term
primP ws = tlocP $
  Prim <$> primInstrP ws


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

parenP :: MonadParser m => m () -> m Term
parenP ws =
  Paren <$> parens ws (termP ws)
  

ifP :: MonadParser m
     => m () -> m Term
ifP ws = tlocP $ do
  P.try $ rsvp ws "if"
  a <- bterm ws
  rsvp ws "then"
  b <- termP ws
  rsvp ws "else"
  c <- termP ws
  return $ If a b c (Annot Nothing)


letP :: MonadParser m
     => m () -> m Term
letP ws = do
  rsvp ws "let"
  x <- varName ws
  equals ws
  a <- termP ws
  rsvp ws "in"
  b <- termP ws
  return $ Let (bind (x, embed a) b)


piP :: MonadParser m => m () -> m Term
piP ws = do
  x <- forallP <|> pure wildcardName
  a <- bterm ws
  arrowr ws
  b <- bterm ws
  return $ Pi (bind (x, embed a) b)
  
  where
    forallP = do
      P.try $ rsvp ws "forall"
      x <- varName ws
      dot ws
      return x


lambdaP :: MonadParser m => m () -> m Term
lambdaP ws = do
  backslash ws
  binds <- P.some (varName ws)
  dot ws
  body <- termP ws
  return $ foldr lam body binds

  where
    lam x m = Lam (bind (x, embed $ Annot Nothing) m)