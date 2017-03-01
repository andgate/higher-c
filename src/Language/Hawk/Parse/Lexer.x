{
{-# LANGUAGE OverloadedStrings #-}
-- Much of this source code was lifted from the Morte library.
module Language.Hawk.Parse.Lexer where

import Control.Monad.Trans.State.Strict (State)
import Data.Binary hiding (encode)
import Data.Bits (shiftR, (.&.))
import Data.Char (digitToInt, ord)
import Data.Text.Lazy (Text)
import Data.Word (Word8)
import Filesystem.Path.CurrentOS (FilePath)
import Language.Hawk.Report.Region (Position (..))
import Pipes (Producer, for, lift, yield)
import Prelude hiding (FilePath)
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.Lazy                   as Text
import qualified Filesystem.Path.CurrentOS        as Filesystem
import qualified Language.Hawk.Syntax.Name        as N
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$digit = 0-9

$blkchar = \:

$opchar = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$opblkchar = [$opchar $blkchar]


$small       = [a-z]
$large       = [A-Z]
$idchar      = [A-Za-z0-9]

$mixfixchar   = [A-Za-z $blkchar]
$mixfixbodychar   = [A-Za-z0-9 $blkchar]


$nonwhite       = ~$white
$whiteNoNewline = $white # \n



-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"



@varid = $small $idchar*
@conid = $large $idchar*
@opid  = $opchar+

-- Blockable id's, these are used by the mifix macro
@mfid    = $mixfixchar $mixfixbodychar* | $opblkchar+

@mixfixa = @mfid \_+ (@mfid \_*)*
@mixfixb = \_+ @mfid (\*_ @mfid)*

@mixfix = @mixfixa | @mixfixb


-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

<0> {
  -- Skip whitespace everywhere
  $whiteNoNewline                 ;
  \n                              { \ _ -> lift startNextLine}
  "//".*                          ;
  
  \"                              { beginString }
  \' .* \'                        { handleChar }
  "/*"                            { beginComment }
  
  \_                              { rsvp }
  \,                              { rsvp }
  \(                              { rsvp }
  \)                              { rsvp }
  \[                              { rsvp }
  \]                              { rsvp }
  \:                              { rsvp }
  
  "case"                          { rsvp }
  "of"                            { rsvp }
  
  @varid                          { \text -> yield (TokenVarId text) }
  @conid                          { \text -> yield (TokenConId text) }
  @opid                           { \text -> yield (TokenOpId text) }
  @mixfix                         { \text -> yield (TokenMixfix text) }
  
  $digit+                         { \text -> yield (TokenInteger $ toInt text) }
}

<stringSC> {
  \\[nt\"]                        { escapeString }
  \"                              { endString }
  .                               { appendString }
}

<commentSC> {
  "/*"                            { beginComment }
  "*/"                            { endComment }
  [.\n]                           ;
}

{


data LexState =
  LexState  { curPos :: Position
            , startcode :: Int
            , commentDepth :: Int
            , stringBuf :: String
            } deriving Show
            
defState :: LexState
defState = LexState (P 0 0) 0 0 ""
            
type Lex = State LexState
  
type LexAction = Text -> Producer TokenClass Lex ()

growColumn :: Int -> Lex ()
growColumn len = do
  s <- State.get
  let (P l c) = curPos s
  State.put s{curPos = (P l (c+len))}
  
startNextLine :: Lex ()
startNextLine = do
  s <- State.get
  let (P l _) = curPos s
  State.put s{curPos = (P (l+1) 0)}


rsvp :: LexAction
rsvp text = yield $ TokenRsvp text


beginString :: LexAction
beginString _ = lift $ do
  s <- State.get
  State.put s{startcode = stringSC}
  
endString :: LexAction
endString _ = do
  s <- lift State.get
  let buf = stringBuf s
  lift $ State.put s{startcode = 0, stringBuf = ""}
  yield (TokenString $ reverse buf)
  
appendString :: LexAction
appendString text = do
  s <- lift State.get
  let c = Text.head text
  lift $ State.put s{stringBuf = c:(stringBuf s)}

escapeString :: LexAction
escapeString text = do
  let c = Text.head $ Text.tail text
      unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  lift $ do
    s <- State.get
    State.put s{stringBuf = unesc:(stringBuf s)}
    

handleChar :: LexAction
handleChar text = do
  let trim = Text.unpack . Text.tail . Text.init
      yeildChar = yield . TokenChar
  case (trim text) of
      ([])   -> yeildChar '\0'
      (c:_)  -> yeildChar '\n'
      "\t"   -> yeildChar '\t'
      "\r"   -> yeildChar '\r'
      "\'"   -> yeildChar '\''
      _      -> return $ error $ "[Lexical Error] Invalid Character Literal: " ++ Text.unpack text


beginComment :: LexAction
beginComment _ = lift $ do
  s <- State.get
  State.put s {startcode = commentSC,
               commentDepth = (commentDepth s)+1}
         
         
endComment :: LexAction         
endComment _ = lift $ do
  s <- State.get
  let cd = commentDepth s
  let sc' = if cd == 1 then 0 else commentSC
  State.put s {startcode = sc', commentDepth = cd-1}


toInt :: Text -> Integer
toInt = Text.foldl' (\x c -> 10 * x + fromIntegral (digitToInt c)) 0

-- This was lifted almost intact from the @alex@ source code
encode :: Char -> (Word8, [Word8])
encode c = (fromIntegral h, map fromIntegral t)
  where
    (h, t) = go (ord c)
    go n
        | n <= 0x7f   = (n, [])
        | n <= 0x7ff  = (0xc0 + (n `shiftR` 6), [0x80 + n .&. 0x3f])
        | n <= 0xffff =
            (   0xe0 + (n `shiftR` 12)
            ,   [   0x80 + ((n `shiftR` 6) .&. 0x3f)
                ,   0x80 + n .&. 0x3f
                ]
            )
        | otherwise   =
            (   0xf0 + (n `shiftR` 18)
            ,   [   0x80 + ((n `shiftR` 12) .&. 0x3f)
                ,   0x80 + ((n `shiftR` 6) .&. 0x3f)
                ,   0x80 + n .&. 0x3f
                ]
            )


{- @alex@ does not provide a `Text` wrapper, so the following code just modifies
   the code from their @basic@ wrapper to work with `Text`

   I could not get the @basic-bytestring@ wrapper to work; it does not correctly
   recognize Unicode regular expressions.
-}
data AlexInput = AlexInput
    { prevChar  :: Char
    , currBytes :: [Word8]
    , currInput :: Text
    }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c bytes text) = case bytes of
    b:ytes -> Just (b, AlexInput c ytes text)
    []     -> case Text.uncons text of
        Nothing       -> Nothing
        Just (t, ext) -> case encode t of
            (b, ytes) -> Just (b, AlexInput t ytes ext)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar


-- | A `Token` augmented with `Position` information
data Token = Token
    { token    :: TokenClass
    , position :: Maybe Position
    } deriving (Show)
    

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
  | TokenOpId Text
  | TokenVarId Text
  | TokenConId Text
  | TokenMixfix Text
  
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
  deriving ( Eq, Show )


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
    TokenRsvp t     -> t
    TokenOpId t     -> t
    TokenVarId t    -> t
    TokenConId t    -> t
    TokenMixfix t   -> t
    TokenInteger i  -> Text.pack $ show i
    TokenDouble d   -> Text.pack $ show d
    TokenChar c     -> Text.pack [c]
    TokenString s   -> Text.pack s
    TokenBool b     -> Text.pack $ show b
    TokenTop        -> ""
    TokenBlk        -> ""
    TokenBlk'       -> ""
    TokenLn         -> ""
    TokenLn'        -> ""
    TokenEof        -> ""
  
  

instance PP.Pretty TokenClass where
  pretty (TokenRsvp t) =
    PP.text "rsvp" <+> PP.text (Text.unpack t)
    
  pretty (TokenOpId t) =
    PP.text "opId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenVarId t) =
    PP.text "varId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenConId t) =
    PP.text "conId" <+> PP.pretty (Text.unpack t)
    
  pretty (TokenMixfix t) =
    PP.text "mixfixId" <+> PP.pretty (Text.unpack t)
    
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
      2 -> TokenOpId <$> get
      3 -> TokenVarId <$> get
      4 -> TokenConId <$> get
      5 -> TokenMixfix <$> get
      6 -> TokenInteger <$> get
      7 -> TokenDouble <$> get
      8 -> TokenChar <$> get
      9 -> TokenString <$> get
      10 -> TokenBool <$> get
      11 -> pure TokenTop
      12 -> pure TokenBlk
      13 -> pure TokenBlk'
      14 -> pure TokenLn
      15 -> pure TokenLn'
      16 -> pure TokenEof
      
  put e =
    case e of
      TokenRsvp t     -> putWord8 1 >> put t
      TokenOpId t     -> putWord8 2 >> put t
      TokenVarId t    -> putWord8 3 >> put t
      TokenConId t    -> putWord8 4 >> put t
      TokenMixfix t   -> putWord8 5 >> put t
      TokenInteger i  -> putWord8 6 >> put i
      TokenDouble d   -> putWord8 7 >> put d
      TokenChar c     -> putWord8 8 >> put c
      TokenString s   -> putWord8 9 >> put s
      TokenBool b     -> putWord8 10 >> put b
      TokenTop        -> putWord8 11
      TokenBlk        -> putWord8 12
      TokenBlk'       -> putWord8 13
      TokenLn         -> putWord8 14
      TokenLn'        -> putWord8 15 
      TokenEof        -> putWord8 16
              
  
          
  

{-| Convert a text representation of a module into a stream of tokens

    `lexModl` keeps track of position and returns the remainder of the input if
    lexing fails.
-}
lexModl :: Text -> Producer Token Lex ()
lexModl text = for (go (AlexInput '\n' [] text)) tag
  where
    tag token = do
        s <- lift State.get
        yield (Token token (Just $ curPos s))

    go input = do
      s <- lift State.get
      case alexScan input (startcode s) of
        AlexEOF                        ->
            yield TokenEof
        AlexError (AlexInput p cs text) ->
            error $ "Lexical Error: Cannot produce token.\n\tPrevious Char: \'" ++ [p] ++ "\'\n\tCurrent Chars: " ++ show cs ++ "\n\tRest of file: " ++ Text.unpack text
        AlexSkip  input' len           -> do
            lift $ growColumn len
            go input'
        AlexToken input' len act       -> do
            act (Text.take (fromIntegral len) (currInput input))
            lift $ growColumn len
            go input'
}
