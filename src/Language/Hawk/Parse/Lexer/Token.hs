{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
  #-}
module Language.Hawk.Parse.Lexer.Token where

import Control.Lens
import Data.Binary hiding (encode)
import Data.Text (Text)
import Language.Hawk.Report.Region (Region(..))

-- -----------------------------------------------------------------------------
-- Token Types

-- | A `Token` augmented with `Region` information
data Token = Token
    { _tokClass     :: TokenClass
    , _tokFilepath  :: FilePath
    , _tokRegion    :: Region
    } deriving (Eq, Show, Ord)

-- The token type:
data TokenClass
  = TokenVarId Text
  | TokenConId Text
  | TokenOpId Text

  | TokenWhiteSpace
  | TokenNewLine

  | TokenCommentLine Text
  | TokenCommentOpen
  | TokenCommentContents Text
  | TokenCommentClose

  | TokenDocCommentLine Text
  | TokenDocCommentOpen
  | TokenDocCommentContents Text
  | TokenDocCommentClose

  | TokenVar
  | TokenVal
  | TokenRef
  | TokenFun
  | TokenSig
  | TokenVow

  | TokenExpose
  | TokenForeign

  | TokenClass
  | TokenInst
  | TokenType
  | TokenNewType
  | TokenData

  | TokenComma
  | TokenPeriod
  | TokenBackslash
  | TokenAt
  | TokenQuestion
  | TokenExclaim
  | TokenColon
  | TokenEqual

  | TokenAmpersand
  | TokenPlus
  | TokenDash
  | TokenStar
  | TokenForwardSlash

  | TokenLArrow
  | TokenRArrow
  | TokenThickLArrow
  | TokenThickRArrow


  | TokenLParen
  | TokenRParen
  | TokenLSquareBracket
  | TokenRSquareBracket
  | TokenLCurlyBracket
  | TokenRCurlyBracket
  | TokenLAngleBracket
  | TokenRAngleBracket
  
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


-- -----------------------------------------------------------------------------
-- Helpers

isSpaceClass :: TokenClass -> Bool
isSpaceClass tc =
  case tc of
    TokenWhiteSpace -> True
    TokenNewLine -> True

    TokenCommentLine _ -> True
    TokenCommentOpen -> True
    TokenCommentContents _ -> True
    TokenCommentClose -> True

    TokenDocCommentLine _ -> True
    TokenDocCommentOpen -> True
    TokenDocCommentContents _ -> True
    TokenDocCommentClose -> True

    _ -> False


isEofClass :: TokenClass -> Bool
isEofClass TokenEof = True
isEofClass _ = False


-- -----------------------------------------------------------------------------
-- Binary Instances

instance Binary Token where
  get =
    Token <$> get <*> get <*> get
      
  put (Token c p r) =
    put c >> put p >> put r


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


