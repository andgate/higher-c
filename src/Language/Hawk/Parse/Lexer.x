{
{-# LANGUAGE   OverloadedStrings
             , TupleSections
             , RankNTypes
             , NoMonomorphismRestriction  -- everyday we stray further from god's light
  #-}

module Language.Hawk.Parse.Lexer where

import Conduit
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT)
import Data.Bits (shiftR, (.&.))
import Data.Char (digitToInt, ord)
import Data.Text (Text)
import Data.Word (Word8)
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Lexer.Layout (layout)
import Language.Hawk.Parse.Lexer.Catalog (catalog)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (Region(..), Position (..))
import System.FilePath (FilePath)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Language.Hawk.Parse.Lexer.Layout as LO
import qualified System.FilePath                  as Filesystem

}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$digit = 0-9

$blkchar = \:

$opchar = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

$small        = [a-z]
$large        = [A-Z]
$idchar       = [A-Za-z0-9]

$nonwhite       = ~$white
$whiteNoNewline = $white # \n



-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"



@varid = $small $idchar*
@conid = $large $idchar*
@opid  = $opchar+


-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

<0> {
  -- Skip whitespace everywhere
  $whiteNoNewline                 { skipBreak }
  \n                              { \ _ _ -> lift nextLineBreak}
  "//".*                          { skipBreak }
  
  \"                              { beginString }
  \' .* \'                        { handleChar }
  "/*"                            { beginComment }
  
  

  \_                              { rsvp }
  \,                              { rsvp }
  \.                              { rsvp }
  \(                              { rsvp }
  \)                              { rsvp }
  \[                              { rsvp }
  \]                              { rsvp }
  \-\>                            { rsvp }
  \=\>                            { rsvp }
  \:                              { rsvp }
  \=                              { rsvp }
  \?                              { rsvp }
  \\                              { rsvp }
  \@                              { rsvp }
  

  "var"                           { rsvp }
  "fun"                           { rsvp }
  "sig"                           { rsvp }
  "class"                         { rsvp }
  "inst"                          { rsvp }
  "type"                          { rsvp }
  "newtype"                       { rsvp }
  "data"                          { rsvp }

  "for"                           { rsvp }
  "foreach"                       { rsvp }
  "while"                         { rsvp }
  "do"                            { rsvp }
  "if"                            { rsvp }
  "then"                          { rsvp }
  "else"                          { rsvp }
  "elif"                          { rsvp }
  "let"                           { rsvp }
  "in"                            { rsvp }
  "case"                          { rsvp }
  "of"                            { rsvp }
  
  
  @varid                          { \text -> yieldTokAt (TokenVarId text) }
  @conid                          { \text -> yieldTokAt (TokenConId text) }
  @opid                           { \text -> yieldTokAt (TokenOpId text) }

  $digit+                         { \text -> yieldTokAt (TokenInteger $ toInt text) }
}

<stringSC> {
  \\[nt\"]                        { escapeString }
  \"                              { endString }
  [.]                             { appendString }
}

<commentSC> {
  "/*"                            { beginComment }
  "*/"                            { endComment }
  \n                              { \ _ _ -> lift nextLineContinue}
  [.]                             { skipContinue }
}

{


data LexState =
  LexState  { curReg :: Region
            , startcode :: Int
            , commentDepth :: Int
            , stringBuf :: String
            } deriving Show
            
defState :: LexState
defState = LexState (R (P 0 0) (P 0 0)) 0 0 ""

type Lex a = forall m. Monad m => StateT LexState m a
type LexAction = forall m. Monad m => Text -> Int -> Producer (StateT LexState m) Token

tag :: TokenClass -> Lex Token
tag tokClass = do
  s <- State.get
  return $ Token tokClass (Just $ curReg s)

resetLex :: Lex ()
resetLex =
  State.put defState

moveRegion :: Int -> Lex ()
moveRegion len = do
  s <- State.get
  let (R _ p1@(P l c)) = curReg s
      p2 = P l (c+len)
  State.put s{curReg = (R p1 p2)}

growRegion :: Int -> Lex ()
growRegion len = do
  s <- State.get
  let (R p1 (P l c)) = curReg s
      p2 = P l (c+len)
  State.put s{curReg = (R p1 p2)}
  
nextLineBreak :: Lex ()
nextLineBreak = do
  s <- State.get
  let (R _ (P l _)) = curReg s
  State.put s{curReg = (R (P (l+1) 0) (P (l+1) 0))}

nextLineContinue :: Lex ()
nextLineContinue = do
  s <- State.get
  let (R p1 (P l _)) = curReg s
  State.put s{curReg = (R p1 (P (l+1) 0))}

yieldTokAt :: forall m. Monad m => TokenClass -> Int -> Producer (StateT LexState m) Token
yieldTokAt c len = do
  lift $ moveRegion len
  yieldTok c

yieldTok :: forall m. Monad m => TokenClass -> Producer (StateT LexState m) Token
yieldTok c = do
  t <- lift $ tag c
  yield t

rsvp :: LexAction
rsvp text =
  yieldTokAt (TokenRsvp text)


skipBreak :: LexAction
skipBreak text len = do
  lift $ moveRegion len

skipContinue :: LexAction
skipContinue text len = do
  lift $ growRegion len

beginString :: LexAction
beginString text len = lift $ do
  moveRegion len
  s <- State.get
  State.put s{startcode = stringSC}
  
endString :: LexAction
endString text len = do
  buf <- lift $ do
    growRegion len
    s <- State.get
    let buf = stringBuf s
    State.put s{ startcode = 0
              , stringBuf = ""
              }
    return buf
  yieldTok (TokenString $ reverse buf)
  
appendString :: LexAction
appendString text len = lift $ do
  growRegion len
  s <- State.get
  let c = Text.head text
      buf = stringBuf s
  State.put s{stringBuf = c:(stringBuf s)}

escapeString :: LexAction
escapeString text len = lift $ do
  let c = Text.head $ Text.tail text
      unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  growRegion len
  s <- State.get
  State.put s{stringBuf = unesc:(stringBuf s)}
    

handleChar :: LexAction
handleChar text len = do
  let trim = Text.unpack . Text.tail . Text.init
      yieldCharAt ch = yieldTokAt (TokenChar ch) len
  case (trim text) of
      ([])   -> yieldCharAt '\0'
      (c:_)  -> yieldCharAt '\n'
      "\t"   -> yieldCharAt '\t'
      "\r"   -> yieldCharAt '\r'
      "\'"   -> yieldCharAt '\''
      _      -> return $ error $ "[Lexical Error] Invalid Character Literal: " ++ Text.unpack text


beginComment :: LexAction
beginComment text len = lift $ do
  moveRegion len
  s <- State.get
  State.put s {startcode = commentSC,
               commentDepth = (commentDepth s)+1}
         
         
endComment :: LexAction      
endComment _ len = lift $ do
  growRegion len
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


{-| Convert a text representation of a module into a stream of tokens

    `lexModl` keeps track of position and returns the remainder of the input if
    lexing fails.
-}
tokenize :: Monad m => Conduit Text m Token
tokenize =
  evalStateC defState $
    awaitForever start
  where
    start text = go (AlexInput '\n' [] text)

    go input = do
      s <- lift State.get
      case alexScan input (startcode s) of
        AlexEOF                        -> do
            yieldTok TokenEof
            return ()
        AlexError (AlexInput p cs text) ->
            error $ "Lexical Error: Cannot produce token.\n\tPrevious Char: \'" ++ [p] ++ "\'\n\tCurrent Chars: " ++ show cs ++ "\n\tRest of file: " ++ Text.unpack text
        AlexSkip  input' len           -> do
            error $ "Lexical Error: default Alex skip should never be invoked."
        AlexToken input' len act       -> do
            act (Text.take (fromIntegral len) (currInput input)) (fromIntegral len)
            go input'

lexer :: Monad m => Conduit TextDoc m TokenDoc
lexer = awaitForever go
  where
    go (Doc mid fp txt) =
      yield txt .| tokenize .| layout .| catalog .| mapC (Doc mid fp)



}
