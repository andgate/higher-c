{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , DeriveGeneric
           , TypeFamilies
           , FlexibleInstances
           , BangPatterns
           , DeriveDataTypeable
  #-}
module Language.Hawk.Lex.Token where

import Control.Lens
import Data.Aeson
import Data.Binary hiding (encode)
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (pretty, (<+>))

import qualified Text.PrettyPrint.Leijen.Text   as PP

-- -----------------------------------------------------------------------------
-- Token Types

-- | A `Token` augmented with `Region` information
data Token = Token
    { _tokClass     :: !TokenClass
    , _tokText      :: !Text
    , _tokLoc       :: !Loc
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- The token type:
data TokenClass
  = TokenRsvp Text
  | TokenPrim Text

  | TokenVarId Text
  | TokenConId Text
  | TokenOpId Text
  
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

instance HasLoc Token where
    loc = tokLoc

instance HasRegion Token where
    region = tokLoc . region

-- -----------------------------------------------------------------------------
-- Pretty Instances

instance PP.Pretty Token where
    pretty t =
      PP.dquotes (PP.textStrict (t^.tokText))
        PP.<+> PP.parens (PP.pretty (t^.tokClass)) 
        PP.<+> PP.textStrict "@"
        PP.<> pretty (t^.tokLoc)

instance PP.Pretty TokenClass where
    pretty tc =
      PP.textStrict (pack . show $ tc)


-- -----------------------------------------------------------------------------
-- Serialization Instances

instance Binary Token
instance FromJSON Token
instance ToJSON Token

instance Binary TokenClass
instance FromJSON TokenClass
instance ToJSON TokenClass
