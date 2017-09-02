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
import Language.Hawk.Parse.Term
import Language.Hawk.Parse.Helpers
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L


declP :: MonadParser m
     => m () -> m Decl
declP ws = 
      sigP ws
  <|> defP ws
  <|> foreignP ws
  <|> fixityP ws


sigP :: MonadParser m
     => m () -> m Decl
sigP ws =
  Sig <$> varName ws <*> typeP ws

fixityP :: MonadParser m => m () -> m Decl
fixityP ws = do
  fx <- P.try $ infixP ws
  p <- fromIntegral <$> integerP ws
  ops <- some (opName ws)

  return $ Fixity fx p ops
    

infixP :: MonadParser m => m() -> m Fixity
infixP ws =
  infixNP ws <|> infixlP ws <|> infixrP ws


infixNP :: MonadParser m
        => m () -> m Fixity
infixNP ws =
  rsvp ws "infix" *> pure InfixN

infixlP :: MonadParser m
        => m () -> m Fixity
infixlP ws =
  rsvp ws "infixl" *> pure InfixL

infixrP :: MonadParser m
        => m () -> m Fixity
infixrP ws =
  rsvp ws "infixr" *> pure InfixR




foreignP :: MonadParser m
         => m () -> m Decl
foreignP ws = 
  Foreign <$> foreignImportP ws

foreignImportP :: MonadParser m
               => m () -> m Foreign
foreignImportP ws = do
  P.try $ do
    rsvp ws "foreign"
    rsvp ws "import"
  ForeignImport <$> foreignTypeP ws <*> stringP' ws <*> varName ws <*> (colon ws *> typeP ws)

foreignTypeP :: MonadParser m
             => m() -> m ForeignType
foreignTypeP ws =
    ForeignC <$ rsvp ws "ccall"


defP :: MonadParser m
     => m () -> m Decl
defP ws =
  Def <$> varName ws <*> defBody
  where
    defBody = foldr lam_ <$> expP ws <*> P.manyTill (varName ws) (equals ws)


