{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
            , LambdaCase
  #-}
module Language.Hawk.Parse.OpTable where

import Control.Applicative
import Data.Text (Text)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax (Operator (..), Fixity (..))


import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P




fixityP :: MonadParser m =>  m (Either [Token] [Operator])
fixityP =
  fmap Right infixDec <|> fmap Left anyLayout
   

infixDec :: MonadParser m => m [Operator]
infixDec = do
  infixDecN <|> infixDecL <|> infixDecR


infixDecN :: MonadParser m => m [Operator]
infixDecN = fixityDec "infix" InfixN

infixDecL :: MonadParser m => m [Operator]
infixDecL = fixityDec "infixl" InfixL

infixDecR :: MonadParser m => m [Operator]
infixDecR = fixityDec "infixr" InfixR


fixityDec :: MonadParser m => Text -> Fixity -> m [Operator]
fixityDec keyword fx = do
  P.try $ rsvp keyword

  p <- fromIntegral <$> integerP
  ops <- some anyOpId

  return $ map (Op fx p) ops

