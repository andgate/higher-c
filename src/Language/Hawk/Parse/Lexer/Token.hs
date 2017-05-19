module Language.Hawk.Parse.Lexer.Token where

import Data.Binary hiding (encode)
import Data.Text (Text)
import Data.Data
import Data.Typeable
import Language.Hawk.Report.Region (Region(..), Position (..))
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))


import qualified Data.Text                        as Text
import qualified Language.Hawk.Syntax.Name        as N
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

-- | A `Token` augmented with `Region` information
data Token = Token
    { token    :: TokenClass
    , region :: Maybe Region
    } deriving (Eq, Show, Ord, Data, Typeable)
    

instance PP.Pretty Token where 
    pretty (Token t p) =
      PP.pretty t <> PP.text "@" <> PP.pretty p 
  

instance Binary Token where
  get =
    Token <$> get <*> get
      
  put (Token t p) =
    put t >> put p
  

-- The token type:
data TokenClass
  = TokenRsvp Text
  | TokenVarId Text
  | TokenConId Text
  | TokenOpId Text
  | TokenMixfixId Text
  | TokenMixfixBlkId Text
  
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
  deriving (Eq, Show, Ord, Data, Typeable)


mkLam :: [N.Source] -> [Token] -> [Token]
mkLam vs (t@(Token TokenBlk _):ts) = t:(mkLam vs ts)
mkLam vs ts = lamOp ++ varIds ++ arrOp ++ ts
  where
    lamOp = [rsvpTok "\\"]
    varIds = map (varIdTok . N.exLocal) vs
    arrOp = [rsvpTok "->"]


rsvpTok :: Text -> Token
rsvpTok txt = Token (TokenRsvp txt) Nothing

varIdTok :: Text -> Token
varIdTok txt = Token (TokenVarId txt) Nothing

tokenToText :: Token -> Text
tokenToText =
  tokenClassToText . token

tokenClassToText :: TokenClass -> Text
tokenClassToText tc =
  case tc of
    TokenRsvp t       -> t
    TokenVarId t      -> t
    TokenConId t      -> t
    TokenOpId t       -> t
    TokenMixfixId t     -> t
    TokenMixfixBlkId t  -> t
    TokenInteger i    -> Text.pack $ show i
    TokenDouble d     -> Text.pack $ show d
    TokenChar c       -> Text.pack [c]
    TokenString s     -> Text.pack s
    TokenBool b       -> Text.pack $ show b
    TokenTop          -> ""
    TokenBlk          -> ""
    TokenBlk'         -> ""
    TokenLn           -> ""
    TokenLn'          -> ""
    TokenEof          -> ""
    
    
tokenToName :: Token -> N.Source
tokenToName (Token tc r) =
  N.Name (tokenClassToText tc) r


isTok :: TokenClass -> Token -> Bool
isTok tc1 t@(Token tc2 _) =
    tc1 == tc2
    
isVar :: Text -> Token -> Bool
isVar txt = isTok $ TokenVarId txt

isCon :: Text -> Token -> Bool
isCon txt = isTok $ TokenConId txt

isOp :: Text -> Token -> Bool
isOp txt = isTok $ TokenOpId txt

isMixfix :: Text -> Token -> Bool
isMixfix txt = isTok $ TokenMixfixId txt

isMixfixBlk :: Text -> Token -> Bool
isMixfixBlk txt = isTok $ TokenMixfixBlkId txt

    
isTokClass :: TokenClass -> Token -> Bool
isTokClass tc1 (Token tc2 _) =
  case (tc1, tc2) of
  
    (TokenRsvp _, TokenRsvp _)    -> True
    (TokenVarId _, TokenVarId _)  -> True
    (TokenConId _, TokenConId _)  -> True
    (TokenOpId _, TokenOpId _)    -> True
    
    (TokenMixfixId _, TokenMixfixId _)        -> True
    (TokenMixfixBlkId _, TokenMixfixBlkId _)  -> True
    
    (TokenInteger _, TokenInteger _)  -> True
    (TokenDouble _, TokenDouble _)    -> True
    (TokenChar _, TokenChar _)        -> True
    (TokenString _, TokenString _)    -> True
    (TokenBool _, TokenBool _)        -> True
    
    (TokenTop, TokenTop)    -> True
    (TokenBlk, TokenBlk)    -> True
    (TokenBlk', TokenBlk')  -> True
    (TokenLn, TokenLn)      -> True
    (TokenLn', TokenLn')    -> True
    (TokenEof, TokenEof)    -> True
    
    _ -> False

isVarId :: Token -> Bool
isVarId = isTokClass $ TokenVarId ""
  
  
isConId :: Token -> Bool
isConId = isTokClass $ TokenConId ""


isOpId :: Token -> Bool
isOpId  = isTokClass $ TokenOpId ""


isMixfixId :: Token -> Bool
isMixfixId  = isTokClass $ TokenMixfixId ""


isMixfixBlkId :: Token -> Bool
isMixfixBlkId  = isTokClass $ TokenMixfixBlkId ""




instance PP.Pretty TokenClass where
  pretty (TokenRsvp t) =
    PP.text "rsvp" <+> PP.text (Text.unpack t)
    
  pretty (TokenVarId t) =
    PP.text "varId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenConId t) =
    PP.text "conId" <+> PP.pretty (Text.unpack t)

  pretty (TokenOpId t) =
    PP.text "opId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenMixfixId t) =
    PP.text "mixfixId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenMixfixBlkId t) =
    PP.text "mixfixblkId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenInteger i) =
    PP.text "int" <+> PP.pretty i
    
  pretty (TokenDouble d) =
    PP.text "dub" <+> PP.pretty  d
    
  pretty (TokenChar c) =
    PP.text "char" <+> PP.pretty c
    
  pretty (TokenString s) =
    PP.text "str" <+> PP.text s
    
  pretty (TokenBool b) =
    PP.text "bool" <+> PP.pretty b
      
  pretty TokenTop =
    PP.text "top"
    
  pretty TokenBlk =
    PP.text "blk"
    
  pretty TokenBlk' =
    PP.text "blk'"
    
  pretty TokenLn =
    PP.text "ln"
    
  pretty TokenLn' =
    PP.text "ln'"
    
  pretty TokenEof =
    PP.text "eof"

instance Binary TokenClass where
  get = do
    n <- getWord8
    case n of
      1 -> TokenRsvp <$> get
      2 -> TokenVarId <$> get
      3 -> TokenConId <$> get
      4 -> TokenOpId <$> get
      5 -> TokenMixfixId <$> get
      6 -> TokenMixfixBlkId <$> get
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
      
  put e =
    case e of
      TokenRsvp t       -> putWord8 1 >> put t
      TokenVarId t      -> putWord8 2 >> put t
      TokenConId t      -> putWord8 3 >> put t
      TokenOpId t       -> putWord8 4 >> put t
      TokenMixfixId t     -> putWord8 5 >> put t
      TokenMixfixBlkId t  -> putWord8 6 >> put t
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