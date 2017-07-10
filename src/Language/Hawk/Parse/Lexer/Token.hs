{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , DeriveGeneric
           , TypeFamilies
           , FlexibleInstances
           , BangPatterns
           , DeriveDataTypeable
  #-}
module Language.Hawk.Parse.Lexer.Token where

import Control.Lens
import Data.Binary hiding (encode)
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (pretty, (<+>))

import qualified Text.Megaparsec.Prim           as P
import qualified Text.Megaparsec.Pos            as P
import qualified Text.PrettyPrint.Leijen.Text   as PP

-- -----------------------------------------------------------------------------
-- Token Types

-- | A `Token` augmented with `Region` information
data Token = Token
    { _tokClass     :: !TokenClass
    , _tokText      :: !Text
    , _tokLoc       :: !Location
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- The token type:
data TokenClass
  = TokenRsvp Text
  | TokenPrim Text

  | TokenVarId Text
  | TokenConId Text
  | TokenModId Text
  | TokenOpId Text
  
  | TokenQVarId Text
  | TokenQConId Text
  | TokenQModId Text
  | TokenQOpId Text

  
  | TokenInteger Integer
  | TokenDouble Double
  | TokenChar Char
  | TokenString String
  | TokenBool Bool
  
  | TokenBlk
  | TokenBlk'
  | TokenLn
  | TokenLn'
  
  | TokenEof
  deriving (Eq, Show, Ord, Data, Typeable, Generic)


makeLenses ''Token

instance HasLocation Token where
    location = tokLoc

instance HasRegion Token where
    region = tokLoc . region

-- -----------------------------------------------------------------------------
-- Pretty Instances

instance PP.Pretty Token where
    pretty t =
      PP.textStrict "Token"
        <+> pretty (t^.tokClass)
        <+> PP.dquotes (PP.textStrict (t^.tokText))
        <+> PP.dquotes (pretty (t^.tokLoc))

instance PP.Pretty TokenClass where
    pretty tc =
      PP.textStrict (pack . show $ tc)


-- -----------------------------------------------------------------------------
-- Binary Instances

instance Binary Token
instance Binary TokenClass


-- -----------------------------------------------------------------------------
-- Megaparsec Stream Instance
type HkToken = Token

instance P.Stream [HkToken] where
    type Token [HkToken] = HkToken
    uncons [] = Nothing
    uncons (t:ts) = Just (t, ts)
    {-# INLINE uncons #-}
    updatePos = const tokUpdatePos
    {-# INLINE updatePos #-}
    

tokUpdatePos
  :: P.Pos               -- ^ Tab width
  -> P.SourcePos         -- ^ Last position
  -> Token               -- ^ Current token
  -> (P.SourcePos, P.SourcePos) -- ^ Last position and Current position
tokUpdatePos _ apos@(P.SourcePos fp _ _) t
  = (apos, npos) -- Don't use megaparsec's position tracking
  where
  npos = p2ps $ t ^. regStart
  {-# INLINE p2ps #-}
  p2ps (P l c) = P.SourcePos fp
                             (P.unsafePos . fromIntegral $ l + 1)
                             (P.unsafePos . fromIntegral $ c + 1)
{-# INLINE tokUpdatePos #-}