{
{-# LANGUAGE   OverloadedStrings
             , TupleSections
             , FlexibleContexts
             , NoMonomorphismRestriction
  #-}

module Language.Hawk.Lex where


import Prelude hiding (lex)
import Control.Lens
import Control.Monad
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Log
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Data.Bits (shiftR, (.&.))
import Data.Bag (Bag (..))
import Data.Char (digitToInt, ord)
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word8)
import Language.Hawk.Load.Result (LdResult, ldFiles)
import Language.Hawk.Lex.Error
import Language.Hawk.Lex.Result (LxResult)
import Language.Hawk.Lex.State
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Location
import System.FilePath (FilePath)

import qualified Data.Map	     as Map
import qualified Data.Text           as T
import qualified Data.Text.Read      as T
import qualified Language.Hawk.Load.Result as LdR
import qualified Language.Hawk.Lex.Format as Fmt
import qualified Language.Hawk.Lex.Result as R
import qualified System.FilePath     as Filesystem

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
  [\n\r]                          { \ _ _ -> nextLineBreak }
  "--|"                           { beginComment }
  "--"                            { beginLineComment }
  
  \"                              { beginString }
  \' .* \'                        { handleChar }
  

  \_                              { rsvp }
  \|                              { rsvp }
  \,                              { rsvp }
  \(                              { rsvp }
  \)                              { rsvp }
  \[                              { rsvp }
  \]                              { rsvp }
  \{                              { rsvp }
  \}                              { rsvp }
  \-\o                            { rsvp }
  \-\>                            { rsvp }
  \=\>                            { rsvp }
  \:\=                            { rsvp }
  \:                              { rsvp }
  \=                              { rsvp }
  \\                              { rsvp }
  \@                              { rsvp }

  "foreign"                       { rsvp }
  "import"                        { rsvp }
  "export"			  { rsvp }
  "ccall"                         { rsvp }
  
  "infix"                         { rsvp }
  "infixl"                        { rsvp }
  "infixr"                        { rsvp }

  "type"                          { rsvp }
  "alias"                         { rsvp }
  "data"                          { rsvp }
  "class"                         { rsvp }  
  "inst"                          { rsvp }

  "do"                            { rsvp }
  "where"                         { rsvp }
  "free"                          { rsvp }
  "dup"				                    { rsvp }
  "so"                            { rsvp }

  "if"                            { rsvp }
  "then"                          { rsvp }
  "else"                          { rsvp }
  "let"                           { rsvp }
  "in"                            { rsvp }
  "case"                          { rsvp }
  "of"                            { rsvp }
  
  @primid                         { \text -> yieldTokAt (TokenPrim text) text }

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
  "--|"                           { continueComment }
  "|--"                           { endComment }
  [\n\r]                          { \_ _ -> nextLineContinue }
  [.]                             { skipContinue }
}

<lineCommentSC> {
  [\n\r]                          { endLineComment }
  [.]                             { skipContinue }
}

{

type LexAction m = Text -> Int -> m ()

tag :: (MonadState s m, HasLexState s)
    => Text -> TokenClass -> m Token
tag text tc = do
  fp <- use lexFilePath
  r <- use lexRegion
  return $ Token tc text (Loc fp r)


moveRegion :: (MonadState s m, HasLexState s, HasRegion s)
           => Int -> m ()
moveRegion len = do
  r1 <- use regEnd
  regStart .= r1 
  regEnd . posColumn += len


growRegion :: (MonadState s m, HasLexState s)
           => Int -> m ()
growRegion len =
  lexRegion . regEnd . posColumn += len

  
nextLineBreak :: (MonadState s m, HasLexState s, HasRegion s)
              => m ()
nextLineBreak = do
  regStart . posLine += 1
  regStart . posColumn .= 0

  regEnd . posLine += 1
  regEnd . posColumn .= 0


nextLineContinue :: (MonadState s m, HasLexState s, HasRegion s)
                 => m ()
nextLineContinue = do
  regEnd . posLine += 1
  regEnd . posColumn .= 0


yieldTokAt :: (MonadState s m, HasLexState s, HasRegion s)
           => TokenClass -> LexAction m
yieldTokAt c text len = do
  moveRegion len
  yieldTaggedTok c text


yieldTaggedTok :: (MonadState s m, HasLexState s, HasRegion s)
               => TokenClass -> Text -> m ()
yieldTaggedTok c text = do
  t <- tag text c
  yieldTok t

yieldTok :: (MonadState s m, HasLexState s, HasRegion s)
         => Token -> m ()
yieldTok t =
  lexTokAcc %= (t:)


rsvp :: (MonadState s m, HasLexState s, HasRegion s)
     => LexAction m
rsvp text =
  yieldTokAt (TokenRsvp text) text


skipBreak :: (MonadState s m, HasLexState s, HasRegion s)
          => LexAction m
skipBreak text len = do
  moveRegion len

skipContinue :: (MonadState s m, HasLexState s, HasRegion s)
             =>  LexAction m
skipContinue text len = do
  growRegion len

beginString :: (MonadState s m, HasLexState s, HasRegion s)
            => LexAction m
beginString text len =
  do
    moveRegion len
    lexStartcode .= stringSC
  
endString :: (MonadState s m, HasLexState s, HasRegion s)
          => LexAction m
endString text len = do
  buf <- do
    growRegion len
    use lexStringBuf

  yieldTaggedTok (TokenString $ reverse buf) text
  
  do
    lexStringBuf .= ""
    lexStartcode .= 0
  
appendString :: (MonadState s m, HasLexState s, HasRegion s)
             => LexAction m
appendString text len =
  do
    growRegion len
    let c = T.head text
    lexStringBuf %= (c:)

escapeString :: (MonadState s m, HasLexState s, HasRegion s)
             => LexAction m
escapeString text len = do
  let c = T.head $ T.tail text
      unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  growRegion len
  lexStringBuf %= (unesc:)

    

handleChar :: ( MonadState s m, HasLexState s, HasRegion s
              , MonadChronicle (Bag e) m, AsLxErr e
              )
           => LexAction m
handleChar text len = do
  let trim = T.unpack . T.tail . T.init
      yieldCharAt ch = yieldTokAt (TokenChar ch) text len
  case (trim text) of
      ([])   -> yieldCharAt '\0'
      (c:_)  -> yieldCharAt '\n'
      "\t"   -> yieldCharAt '\t'
      "\r"   -> yieldCharAt '\r'
      "\'"   -> yieldCharAt '\''
      _      -> disclose $ One (_InvalidCharLit # text)


beginComment :: (MonadState s m, HasLexState s, HasRegion s)
             => LexAction m
beginComment text len =
  do
    moveRegion len
    lexStartcode .= commentSC
    lexCommentDepth .= 1

continueComment :: (MonadState s m, HasLexState s, HasRegion s)
                => LexAction m
continueComment text len =
  do
    growRegion len
    lexCommentDepth += 1
         
         
endComment :: (MonadState s m, HasLexState s, HasRegion s)
           => LexAction m
endComment _ len =
  do
    growRegion len
    
    lexCommentDepth -= 1
    cd <- use lexCommentDepth

    lexStartcode .=
      if cd == 0
        then 0
        else commentSC

beginLineComment :: (MonadState s m, HasLexState s, HasRegion s)
                 => LexAction m
beginLineComment text len =
  do
    moveRegion len
    lexStartcode .= lineCommentSC

endLineComment :: (MonadState s m, HasLexState s, HasRegion s)
               => LexAction m
endLineComment text len =
  do
    nextLineContinue
    lexStartcode .= 0


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



lexMany :: ( MonadChronicle (Bag e) m, AsLxErr e )
        => LdResult -> m LxResult
lexMany =
  mconcatMapM (uncurry lex) . LdR.toList


-- Simple token lexing
lex :: ( MonadChronicle (Bag e) m, AsLxErr e )
      => FilePath -> Text -> m LxResult
lex fp text = do
  toks <- evalStateT (start text)
                     ((def :: LexState) & lexFilePath .~ fp)
  return . Fmt.layout $ R.singleton fp [toks]

  where
    start = go . AlexInput '\n' []

    go input = do
      sc <- use lexStartcode
      case alexScan input sc of
        AlexEOF                         -> do
            yieldTaggedTok TokenEof ""
            reverse <$> use lexTokAcc

        AlexError (AlexInput p cs text) -> do
	    fp <- use lexFilePath
	    r  <- use lexRegion
	    let l = Loc fp r
            disclose $ One (_UnproducibleToken # (show cs, l))
        
        AlexSkip  input' len           -> do
            disclose $ One (_IllegalLexerSkip # ())
        
        AlexToken input' len act       -> do
            act (T.take (fromIntegral len) (currInput input)) (fromIntegral len)
            go input'

}
