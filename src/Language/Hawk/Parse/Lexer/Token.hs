{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
  #-}
module Language.Hawk.Parse.Lexer.Token where

import Control.Lens
import Data.Binary hiding (encode)
import Data.Text (Text, pack)
import Language.Hawk.Report.SrcLoc (SrcLoc(..), HasSrcLoc(..))
import Language.Hawk.Report.Region (Region(..), HasRegion(..))
import Text.PrettyPrint.Leijen.Text (pretty, (<+>))

import qualified Text.PrettyPrint.Leijen.Text     as PP

-- -----------------------------------------------------------------------------
-- Token Types

-- | A `Token` augmented with `Region` information
data Token = Token
    { _tokClass     :: TokenClass
    , _tokText      :: Text
    , _tokLoc       :: SrcLoc
    } deriving (Eq, Show, Ord)

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
  
  | TokenTop
  | TokenBlk
  | TokenBlk'
  | TokenLn
  | TokenLn'
  
  | TokenEof
  deriving (Eq, Show, Ord)


makeLenses ''Token

instance HasRegion Token where
    region = tokLoc . region

instance HasSrcLoc Token where
    srcLoc = tokLoc

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

instance Binary Token where
  get =
    Token <$> get <*> get <*> get
      
  put (Token c t loc) =
    put c >> put t >> put loc


instance Binary TokenClass where
  get = do
    n <- getWord8
    case n of
      2 -> TokenVarId <$> get
      3 -> TokenConId <$> get
      4 -> TokenOpId <$> get 

      7 -> TokenInteger <$> get
      8 -> TokenDouble <$> get
      9 -> TokenChar <$> get
      10 -> TokenString <$> get
      11 -> TokenBool <$> get
      12 -> pure TokenTop
      13 -> pure TokenBlk
      14 -> pure TokenBlk'
      15 -> pure TokenLn
      16 -> pure TokenLn'
      17 -> pure TokenEof
      _  -> error "data corrupted"
      
  put e =
    case e of
      TokenVarId t      -> putWord8 2 >> put t
      TokenConId t      -> putWord8 3 >> put t
      TokenOpId t       -> putWord8 4 >> put t
      TokenInteger i    -> putWord8 7 >> put i
      TokenDouble d     -> putWord8 8 >> put d
      TokenChar c       -> putWord8 9 >> put c
      TokenString s     -> putWord8 10 >> put s
      TokenBool b       -> putWord8 11 >> put b
      TokenTop          -> putWord8 12
      TokenBlk          -> putWord8 13
      TokenBlk'         -> putWord8 14
      TokenLn           -> putWord8 15
      TokenLn'          -> putWord8 16 
      TokenEof          -> putWord8 17


