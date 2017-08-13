{-# LANGUAGE  OverloadedStrings
            , ConstraintKinds
            , FlexibleContexts
            , FlexibleInstances
            , TypeFamilies
            , BangPatterns
            , RankNTypes
  #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Lens hiding (op)
import Data.List.NonEmpty (NonEmpty (..))
import Data.IntMap (IntMap)
import Data.Monoid
import Data.Text (Text, pack)
import Language.Hawk.Syntax
import Language.Hawk.Parse.Lexer.Token

import Text.Megaparsec.Prim (MonadParsec(..))

import qualified Data.IntMap.Lazy           as IMap
import qualified Data.Set                   as Set
import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P
import qualified Text.Megaparsec.Error      as P
import qualified Text.Megaparsec.Expr       as P

-- -----------------------------------------------------------------------------
-- Parser Monad Class

type MonadParser m = (Functor m, Applicative m, Monad m, MonadParsec P.Dec [Token] m)

type ExpParser = P.Parsec P.Dec [Token]

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

opId :: MonadParser m => Text -> m Text
opId name
  = (^.tokText) <$> op name

-- -----------------------------------------------------------------------------
-- Grouping Helpers

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
-- Seperators Helpers

commaSep1 :: MonadParser m => m a -> m [a]
commaSep1 = (`P.sepBy1` rsvp ",")

-- -----------------------------------------------------------------------------
-- Id Name Helpers 

anyVarName :: MonadParser m => m Name
anyVarName = Name <$> anyVarId

anyConName :: MonadParser m => m Name
anyConName = Name <$> anyConId

-- -----------------------------------------------------------------------------
-- Id Text Helpers 

anyVarId :: MonadParser m => m Text
anyVarId = (^.tokText) <$> anyVarT

anyConId :: MonadParser m => m Text
anyConId = anyModId <|> anyConId'

anyConId' :: MonadParser m => m Text
anyConId' = (^.tokText) <$> anyConT

anyModId :: MonadParser m => m Text
anyModId = (^.tokText) <$> anyModT

anyOpId :: MonadParser m => m Text
anyOpId = (^.tokText) <$> anyOpT


-- -----------------------------------------------------------------------------
-- Id Token Helpers

anyVarT :: MonadParser m => m Token
anyVarT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenVarId _  -> True
        _             -> False

anyConT :: MonadParser m => m Token
anyConT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenConId _  -> True
        _             -> False

anyModT :: MonadParser m => m Token
anyModT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenModId _  -> True
        _             -> False


anyOpT :: MonadParser m => m Token
anyOpT
  = satisfyT $ \t ->
      case t^.tokClass of
        TokenOpId _  -> True
        _             -> False


{-# INLINE integerP #-}
integerP :: MonadParser m => m Integer
integerP
  = P.token testTok Nothing
  where
    {-# INLINE testTok #-}
    testTok t =
      case t^.tokClass of
        (TokenInteger x) -> Right x
        _ -> Left (Set.singleton (P.Tokens (t:|[])), Set.empty, Set.empty)


{-# INLINE doubleP #-}
doubleP :: MonadParser m => m Double
doubleP
  = P.token testTok Nothing
  where
    {-# INLINE testTok #-}
    testTok t =
      case t^.tokClass of
        (TokenDouble x) -> Right x
        _ -> Left (Set.singleton (P.Tokens (t:|[])), Set.empty, Set.empty)


{-# INLINE charP #-}
charP :: MonadParser m => m Char
charP
  = P.token testTok Nothing
  where
    {-# INLINE testTok #-}
    testTok t =
      case t^.tokClass of
        (TokenChar c) -> Right c
        _ -> Left (Set.singleton (P.Tokens (t:|[])), Set.empty, Set.empty)



{-# INLINE stringP #-}
stringP :: MonadParser m => m Text
stringP
  = P.token testTok Nothing
  where
    {-# INLINE testTok #-}
    testTok t =
      case t^.tokClass of
        (TokenString x) -> Right (pack x)
        _ -> Left (Set.singleton (P.Tokens (t:|[])), Set.empty, Set.empty)

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
-- Location Helpers

peekLoc :: MonadParser m => m Location
peekLoc =
  (^.tokLoc) <$> P.lookAhead anyT

peekIndent :: MonadParser m => m Int
peekIndent =
  (^.regStart.posColumn) <$> P.lookAhead anyT


locP :: MonadParser m
     => m a -> m (L a)
locP p = do
  l1 <- peekLoc
  x <- p
  withRecovery (\_ -> return $ L l1 x) $ do
    l2 <- peekLoc
    return $ L (l1 <> l2) x

-- | Wrap an expression with a location
elocP :: MonadParser m
        => m (Exp Var) -> m (Exp Var)
elocP p = do
  l1 <- peekLoc
  e <- p
  withRecovery (\_ -> return $ ELoc l1 e) $ do
    l2 <- peekLoc
    return $ ELoc (l1 <> l2) e


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


splitLinefolds :: MonadParser m => m [[Token]]
splitLinefolds = linefolds anyLayout
