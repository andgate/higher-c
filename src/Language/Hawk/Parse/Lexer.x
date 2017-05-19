{
{-# LANGUAGE   OverloadedStrings
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
import Language.Hawk.Metadata.Schema (ModuleId)
import Language.Hawk.Parse.Lexer.Layout (layout)
import Language.Hawk.Parse.Lexer.Catalog
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (Position (..))
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
$opblkchar = [$opchar $blkchar]


$small        = [a-z]
$large        = [A-Z]
$idchar       = [A-Za-z0-9]

$blkchar      = [A-Za-z $blkchar]
$blkbodychar  = [A-Za-z0-9 $blkchar]

$nonwhite       = ~$white
$whiteNoNewline = $white # \n



-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"



@varid = $small $idchar*
@conid = $large $idchar*
@opid  = $opchar+


-- Blockable id's, these are used by the mifix macro
@id    = @varid | @conid | @opid

@mixfixA = \_+ @id
@mixfixB = @id \_+
@mixfixC = \_+ @id \_+
@mixfix = (@mixfixA | @mixfixB | @mixfixC)+

-- Blockable id's, these are used by the mifix macro
@blkid    = $blkchar $blkbodychar* | $opblkchar+

@mixfixblkA = \_+ @blkid
@mixfixblkB = @blkid \_+
@mixfixblkC = \_+ @blkid \_+
@mixfixblk = (@mixfixblkA | @mixfixblkB | @mixfixblkC)+


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
  \.                              { rsvp }
  \(                              { rsvp }
  \)                              { rsvp }
  \[                              { rsvp }
  \]                              { rsvp }
  \-\>                            { rsvp }
  \=\>                            { rsvp }
  \:\=                            { rsvp }
  \:\-                            { rsvp }
  \:\~                            { rsvp }
  \:                              { rsvp }      
  \-                              { rsvp }
  \=                              { rsvp }
  \?                              { rsvp }
  \\                              { rsvp }
  \@                              { rsvp }
  
  "case"                          { rsvp }
  "of"                            { rsvp }
  
  
  @varid                          { \text -> yield (TokenVarId text) }
  @conid                          { \text -> yield (TokenConId text) }
  @opid                           { \text -> yield (TokenOpId text) }
  @mixfix                         { \text -> yield (TokenMixfixId text) }
  @mixfixblk                      { \text -> yield (TokenMixfixBlkId text) }

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
defState = LexState (P 1 1) 0 0 ""

type Lex a = forall m. Monad m => StateT LexState m a

type LexAction = forall m. Monad m => Text -> Producer (StateT LexState m) TokenClass


resetLex :: Lex ()
resetLex =
  State.put defState


growColumn :: Int -> Lex ()
growColumn len = do
  s <- State.get
  let (P l c) = curPos s
  State.put s{curPos = (P l (c+len))}
  
startNextLine :: Lex ()
startNextLine = do
  s <- State.get
  let (P l _) = curPos s
  State.put s{curPos = (P (l+1) 1)}


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


{-| Convert a text representation of a module into a stream of tokens

    `lexModl` keeps track of position and returns the remainder of the input if
    lexing fails.
-}
tokenize :: Monad m => Conduit Text m Token
tokenize =
  evalStateC defState $
    awaitForever start .| mapMC tag
  where
    start text = go (AlexInput '\n' [] text)

    tag :: TokenClass -> Lex Token
    tag tokClass = do
      s <- State.get
      return $ Token tokClass (Just $ curPos s)

    go input = do
      s <- lift State.get
      case alexScan input (startcode s) of
        AlexEOF                        -> do
            yield TokenEof
            return ()
        AlexError (AlexInput p cs text) ->
            error $ "Lexical Error: Cannot produce token.\n\tPrevious Char: \'" ++ [p] ++ "\'\n\tCurrent Chars: " ++ show cs ++ "\n\tRest of file: " ++ Text.unpack text
        AlexSkip  input' len           -> do
            lift $ growColumn len
            go input'
        AlexToken input' len act       -> do
            lift $ growColumn len
            act (Text.take (fromIntegral len) (currInput input))
            go input'

lexer :: Monad m => Conduit (Text, ModuleId) m (Token, ModuleId)
lexer = awaitForever go
  where
    go (txt, mid) =
      yield txt .| tokenize .| layout .| (mapC (\tok -> (tok, mid)))



}
