{-# LANGUAGE  OverloadedStrings
            , ConstraintKinds
            , FlexibleContexts
            , FlexibleInstances
            , TypeFamilies
            , BangPatterns
  #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Language.Hawk.Parse.Lexer.Token

import Text.Megaparsec.Prim (MonadParsec(..))

import qualified Data.Set                   as Set
import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P
import qualified Text.Megaparsec.Error      as P

-- -----------------------------------------------------------------------------
-- Parser Monad Class

type MonadParser m = (Functor m, Applicative m, Monad m, MonadParsec P.Dec [Token] m)


-- -----------------------------------------------------------------------------
-- Token Parser Helpers

{-# INLINE satisfyT #-}
satisfyT :: MonadParser m => (Token -> Bool) -> m Token
satisfyT f
  = P.token testTok Nothing
  where
    {-# INLINE testTok #-}
    testTok t =
      if f t
        then Right t
        else Left (Set.singleton (P.Tokens (t:|[])), Set.empty, Set.empty)



matchT :: MonadParser m => TokenClass -> m Token
matchT tc
  = satisfyT q
  where
    q t = 
      tc == t^.tokClass


anyT :: MonadParser m => m Token
anyT = satisfyT $ const True


exceptT :: MonadParser m => TokenClass -> m Token
exceptT tc
   = satisfyT q
   where
     q t = 
      tc /= t^.tokClass


-- -----------------------------------------------------------------------------
-- Terminal Helpers

rsvp :: MonadParser m => Text -> m Token
rsvp
  = matchT . TokenRsvp 


prim :: MonadParser m => Text -> m Token
prim
  = matchT . TokenPrim 


op :: MonadParser m => Text -> m Token
op
  = matchT . TokenOpId


-- -----------------------------------------------------------------------------
-- Combinator Helpers

parens :: MonadParser m => m a -> m a
parens p
  = rsvp "(" *> p <* rsvp ")"

sqrBrackets :: MonadParser m => m a -> m a
sqrBrackets p
  = rsvp "[" *> p <* rsvp "]"

curlyBrackets :: MonadParser m => m a -> m a
curlyBrackets p
  = rsvp "{" *> p <* rsvp "}"

angleBrackets :: MonadParser m => m a -> m a
angleBrackets p
  = rsvp "<" *> p <* rsvp ">"


-- -----------------------------------------------------------------------------
-- Id Helpers

-- Id Text
varId :: MonadParser m => m Text
varId = (^.tokText) <$> varT

conId :: MonadParser m => m Text
conId = modId <|> conId'

conId' :: MonadParser m => m Text
conId' = (^.tokText) <$> conT

modId :: MonadParser m => m Text
modId = (^.tokText) <$> modT

opId :: MonadParser m => m Text
opId = (^.tokText) <$> opT



-- Id tokens
varT :: MonadParser m => m Token
varT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenVarId _  -> True
        _             -> False

conT :: MonadParser m => m Token
conT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenConId _  -> True
        _             -> False

modT :: MonadParser m => m Token
modT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenModId _  -> True
        _             -> False


opT :: MonadParser m => m Token
opT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenOpId _  -> True
        _             -> False

-- -----------------------------------------------------------------------------
-- Layout Helpers

block :: MonadParser m => m a -> m [a]
block p = blk *> linefolds p <* blk'

linefolds0 :: MonadParser m => m a -> m [a]
linefolds0 p = many $ linefold p

linefolds :: MonadParser m => m a -> m [a]
linefolds p = some $ linefold p

linefold :: MonadParser m => m a -> m a
linefold p = ln *> p <* ln'


blk :: MonadParser m => m Token
blk = matchT TokenBlk

blk' :: MonadParser m => m Token
blk' = matchT TokenBlk'


ln :: MonadParser m => m Token
ln = matchT TokenLn P.<?> "linefold open"

ln' :: MonadParser m => m Token
ln' = matchT TokenLn' P.<?> "linefold close"


eof :: MonadParser m => m Token
eof = matchT TokenEof


-- -----------------------------------------------------------------------------
-- Token Grouping Helpers

{-# INLINE exceptLayout #-}
exceptLayout :: MonadParser m => m Token
exceptLayout
  = satisfyT $ \t ->
      t^.tokClass `notElem` [TokenLn, TokenLn', TokenBlk, TokenBlk']


{-# INLINE anyLayout #-}
anyLayout :: MonadParser m => m [Token]
anyLayout = concat <$> some anyLayout'

{-# INLINE anyLayout' #-}
anyLayout' :: MonadParser m => m [Token]
anyLayout' = try (some exceptLayout) <|> try anyLine <|> anyBlock


{-# INLINE anyLine #-}
anyLine :: MonadParser m => m [Token]
anyLine = do
  t1 <- ln
  ts <- anyLayout
  t2 <- ln'
  return $ t1:(ts ++ [t2])


{-# INLINE anyBlock #-}
anyBlock :: MonadParser m => m [Token]
anyBlock = do
  t1 <- blk
  ts <- anyLayout
  t2 <- blk'
  return $ t1:(ts ++ [t2])