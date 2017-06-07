{
{-# LANGUAGE   OverloadedStrings
             , TupleSections
             , RankNTypes
             , NoMonomorphismRestriction  -- everyday we stray further from god's light
  #-}

module Language.Hawk.Parse.Lexer where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State.Strict (StateT)
import Data.Bits (shiftR, (.&.))
import Data.Char (digitToInt, ord)
import Data.Text (Text)
import Data.Word (Word8)
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Lexer.State
import Language.Hawk.Parse.Lexer.Layout (layout)
import Language.Hawk.Parse.Lexer.Catalog (catalog)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region
import System.FilePath (FilePath)

import qualified Data.Text                        as T
import qualified Data.Text.Read                   as T
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
  \{                              { rsvp }
  \}                              { rsvp }
  \<                              { rsvp }
  \>                              { rsvp }
  \-\>                            { rsvp }
  \=\>                            { rsvp }
  \:                              { rsvp }
  \=                              { rsvp }
  \?                              { rsvp }
  \\                              { rsvp }
  \@                              { rsvp }

  "expose"                        { rsvp }
  "foreign"                       { rsvp }
  "ccall"                         { rsvp }   
  
  "vow"                           { rsvp }
  "var"                           { rsvp }
  "val"                           { rsvp }
  "ref"                           { rsvp }
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
  
  
  @varid                          { \text -> yieldTokAt text (TokenVarId text) }
  @conid                          { \text -> yieldTokAt text  (TokenConId text) }
  @opid                           { \text -> yieldTokAt text (TokenOpId text) }

  [\+\-] $digit* \. $digit+       { \text -> yieldTokAt text (TokenDouble $ readSignedDbl text) }
  [\+\-] $digit+ \. $digit*       { \text -> yieldTokAt text (TokenDouble $ readSignedDbl text) }
  $digit* \. $digit+              { \text -> yieldTokAt text (TokenDouble $ readDbl text) }
  $digit+ \. $digit*              { \text -> yieldTokAt text (TokenDouble $ readDbl text) }
  
  $digit+                         { \text -> yieldTokAt text (TokenInteger $ readInt text) }
  [\+\-] $digit+                  { \text -> yieldTokAt text (TokenInteger $ readSignedInt text) }
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

type Lex a = forall m. Monad m => StateT LexState m a
type LexAction = forall m. Monad m => Text -> Int -> Producer (StateT LexState m) Token

tag :: Text -> TokenClass -> Lex Token
tag text tc = do
  fp <- use lexFilePath
  r <- use lexRegion
  return $ Token tc text fp r


moveRegion :: Int -> Lex ()
moveRegion len =
  zoom lexRegion $ do
    r1 <- use regEnd
    regStart .= r1 
    regEnd . posColumn += len


growRegion :: Int -> Lex ()
growRegion len =
  lexRegion . regEnd . posColumn += len

  
nextLineBreak :: Lex ()
nextLineBreak =
  zoom lexRegion $ do
    zoom regStart $ do
      posLine += 1
      posColumn .= 0
    
    zoom regEnd $ do
      posLine += 1
      posColumn .= 0


nextLineContinue :: Lex ()
nextLineContinue =
  zoom (lexRegion . regEnd) $ do
    posLine += 1
    posColumn .= 0


yieldTokAt :: forall m. Monad m => Text -> TokenClass -> Int -> Producer (StateT LexState m) Token
yieldTokAt text c len = do
  lift $ moveRegion len
  yieldTok text c


yieldTok :: forall m. Monad m => Text -> TokenClass -> Producer (StateT LexState m) Token
yieldTok text c = do
  t <- lift $ tag text c
  yield t


rsvp :: LexAction
rsvp text =
  yieldTokAt text (TokenRsvp text)


skipBreak :: LexAction
skipBreak text len = do
  lift $ moveRegion len

skipContinue :: LexAction
skipContinue text len = do
  lift $ growRegion len

beginString :: LexAction
beginString text len =
  lift $ do
    moveRegion len
    lexStartcode .= stringSC
  
endString :: LexAction
endString text len = do
  buf <- lift $ do
    growRegion len
    use lexStringBuf

  yieldTok text (TokenString $ reverse buf)
  
  lift $ do
    lexStringBuf .= ""
    lexStartcode .= 0
  
appendString :: LexAction
appendString text len =
  lift $ do
    growRegion len
    let c = T.head text
    lexStringBuf %= (c:)

escapeString :: LexAction
escapeString text len = lift $ do
  let c = T.head $ T.tail text
      unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  growRegion len
  lexStringBuf %= (unesc:)

    

handleChar :: LexAction
handleChar text len = do
  let trim = T.unpack . T.tail . T.init
      yieldCharAt ch = yieldTokAt text (TokenChar ch) len
  case (trim text) of
      ([])   -> yieldCharAt '\0'
      (c:_)  -> yieldCharAt '\n'
      "\t"   -> yieldCharAt '\t'
      "\r"   -> yieldCharAt '\r'
      "\'"   -> yieldCharAt '\''
      _      -> return $ error $ "[Lexical Error] Invalid Character Literal: " ++ T.unpack text


beginComment :: LexAction
beginComment text len =
  lift $ do
    moveRegion len
    lexStartcode .= commentSC
    lexCommentDepth += 1
         
         
endComment :: LexAction      
endComment _ len =
  lift $ do
    growRegion len
    
    lexCommentDepth -= 1
    cd <- use lexCommentDepth

    lexStartcode .=
      if cd == 0
        then 0
        else commentSC


forceRight :: Either a b -> b
forceRight (Right b) = b
forceRight _ = undefined

readInt :: Text -> Integer
readInt = fst . forceRight . T.decimal

readSignedInt :: Text -> Integer
readSignedInt = fst . forceRight . T.signed T.decimal

readDbl :: Text -> Double
readDbl = fst . forceRight . T.double

readSignedDbl :: Text -> Double
readSignedDbl = fst . forceRight . T.signed T.double

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
    []     -> case T.uncons text of
        Nothing       -> Nothing
        Just (t, ext) -> case encode t of
            (b, ytes) -> Just (b, AlexInput t ytes ext)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar              


{-| Convert a text representation of a module into a stream of tokens

    `lexModl` keeps track of position and returns the remainder of the input if
    lexing fails.
-}
tokenize :: Monad m => FilePath -> Conduit Text m Token
tokenize fp =
  evalStateC (defState fp) $
    awaitForever start
  where
    start text = go (AlexInput '\n' [] text)

    go input = do
      sc <- lift $ use lexStartcode
      case alexScan input sc of
        AlexEOF                        -> do
            yieldTok "" TokenEof

        AlexError (AlexInput p cs text) ->
            error $ "Lexical Error: Cannot produce token.\n\tPrevious Char: \'" ++ [p] ++ "\'\n\tCurrent Chars: " ++ show cs ++ "\n\tRest of file: " ++ T.unpack text
        
        AlexSkip  input' len           -> do
            error $ "Lexical Error: default Alex skip should never be invoked."
        
        AlexToken input' len act       -> do
            act (T.take (fromIntegral len) (currInput input)) (fromIntegral len)
            go input'

lexer :: Monad m => Conduit TextDoc m TokenDoc
lexer = awaitForever go
  where
    go (Doc mid fp txt) =
      yield txt .| tokenize fp .| layout .| catalog .| mapC (Doc mid fp)



}
