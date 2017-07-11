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
      (DecItem <$> decP)
  <|> (DefItem <$> defP ops)



decP :: MonadParser m
     => m Dec
decP =
  Dec <$> (varnameP <* rsvp ":") <*> typeP


defP :: MonadParser m
     => ExpOpTable m -> m Def
defP ops =
  Def <$> varnameP <*>  many patP <*> expP ops
  

varnameP :: MonadParser m => m Name
varnameP = Name <$> anyVarId

varP :: MonadParser m => m Var
varP = Var <$> anyVarId

patP :: MonadParser m => m Pat
patP =
  Pat <$> anyVarId


expP :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
expP ops = undefined


cexp :: MonadParser m
     => ExpOpTable m -> m (Exp Var)
cexp ops = P.makeExprParser aexpP ops
  

aexpP :: MonadParser m => m (Exp Var)
aexpP = evarP


evarP :: MonadParser m => m (Exp Var)
evarP = EVar <$> varP


typeP :: MonadParser m => m Type
typeP = undefined