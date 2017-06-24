{
{-# LANGUAGE   OverloadedStrings
             , TupleSections
  #-}

module Language.Hawk.Parse.Lexer where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict (State, evalState)
import Data.Bits (shiftR, (.&.))
import Data.Char (digitToInt, ord)
import Data.Default.Class
import Data.Text (Text)
import Data.Word (Word8)
import Language.Hawk.Parse.Lexer.State
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


$opchar = [\!\#\$\%\&\*\+\/\<\=\>\?\@\\\^\|\-\~\:]

$small        = [a-z]
$large        = [A-Z]
$idchar       = [A-Za-z0-9]
$idcharsym    = [A-Za-z0-9\_\']


$nonwhite       = ~$white
$whiteNoNewline = $white # \n


-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

-- Basic Ids
@primid = \# $small+
@modid = $large $idchar*
@varid = $small $idcharsym*
@conid = $large $idcharsym*
@opid  = $opchar+


-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

<0> {
  -- Skip whitespace everywhere
  $whiteNoNewline                 { skipBreak }
  \n                              { \ _ _ -> nextLineBreak}
  "//".*                          { skipBreak }
  
  \"                              { beginString }
  \' .* \'                        { handleChar }
  "/*"                            { beginComment }
  

  \_                              { rsvp }
  \.                              { rsvp }
  \,                              { rsvp }
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
  
  "infix"                         { rsvp }
  "infixl"                        { rsvp }
  "infixr"                        { rsvp }

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

  "do"                            { rsvp }
  "return"                        { rsvp }
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
  
  @primid                         { \text -> yieldTokAt (TokenPrim text) text }

  @modid                          { \text -> yieldTokAt (TokenModId text) text }
  @conid                          { \text -> yieldTokAt (TokenConId text) text }
  @varid                          { \text -> yieldTokAt (TokenVarId text) text }
  @opid                           { \text -> yieldTokAt (TokenOpId text) text }

  $digit* \. $digit+              { \text -> yieldTokAt (TokenDouble $ readDbl text) text }
  $digit+ \. $digit*              { \text -> yieldTokAt (TokenDouble $ readDbl text) text }
  
  $digit+                         { \text -> yieldTokAt (TokenInteger $ readInt text) text }
}

<stringSC> {
  \\[nt\"]                        { escapeString }
  \"                              { endString }
  [.]                             { appendString }
}

<commentSC> {
  "/*"                            { beginComment }
  "*/"                            { endComment }
  \n                              { \ _ _ -> nextLineContinue}
  [.]                             { skipContinue }
}

{

type Lex a = State LexState a
type LexAction = Text -> Int -> Lex ()

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


yieldTokAt :: TokenClass -> LexAction
yieldTokAt c text len = do
  moveRegion len
  yieldTaggedTok c text


yieldTaggedTok :: TokenClass -> Text -> Lex ()
yieldTaggedTok c text = do
  t <- tag text c
  yieldTok t

yieldTok :: Token -> Lex ()
yieldTok t =
  lexTokAcc %= (t:)


rsvp :: LexAction
rsvp text =
  yieldTokAt (TokenRsvp text) text


skipBreak :: LexAction
skipBreak text len = do
  moveRegion len

skipContinue :: LexAction
skipContinue text len = do
  growRegion len

beginString :: LexAction
beginString text len =
  do
    moveRegion len
    lexStartcode .= stringSC
  
endString :: LexAction
endString text len = do
  buf <- do
    growRegion len
    use lexStringBuf

  yieldTaggedTok (TokenString $ reverse buf) text
  
  do
    lexStringBuf .= ""
    lexStartcode .= 0
  
appendString :: LexAction
appendString text len =
  do
    growRegion len
    let c = T.head text
    lexStringBuf %= (c:)

escapeString :: LexAction
escapeString text len = do
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
      yieldCharAt ch = yieldTokAt (TokenChar ch) text len
  case (trim text) of
      ([])   -> yieldCharAt '\0'
      (c:_)  -> yieldCharAt '\n'
      "\t"   -> yieldCharAt '\t'
      "\r"   -> yieldCharAt '\r'
      "\'"   -> yieldCharAt '\''
      _      -> return $ error $ "[Lexical Error] Invalid Character Literal: " ++ T.unpack text


beginComment :: LexAction
beginComment text len =
  do
    moveRegion len
    lexStartcode .= commentSC
    lexCommentDepth += 1
         
         
endComment :: LexAction
endComment _ len =
  do
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
lexer :: FilePath -> Text -> [Token]
lexer fp text =
    evalState (go (AlexInput '\n' [] text)) (def & lexFilePath .~ fp)

  where
    start text = go (AlexInput '\n' [] text)

    go input = do
      sc <- use lexStartcode
      case alexScan input sc of
        AlexEOF                        -> do
            yieldTaggedTok TokenEof ""
            reverse <$> use lexTokAcc

        AlexError (AlexInput p cs text) ->
            -- This is why we need ExceptT or ChronicleT
            error $ "Lexical Error: Cannot produce token.\n\tPrevious Char: \'" ++ [p] ++ "\'\n\tCurrent Chars: " ++ show cs ++ "\n\tRest of file: " ++ T.unpack text
        
        AlexSkip  input' len           -> do
            -- This is another reason for ExceptT or ChronicleT
            error $ "Lexical Error: default Alex skip should never be invoked."
        
        AlexToken input' len act       -> do
            act (T.take (fromIntegral len) (currInput input)) (fromIntegral len)
            go input'

}
