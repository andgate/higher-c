{
{-# LANGUAGE OverloadedStrings #-}
-- Much of this source code was lifted from the Morte library.
module Language.Hawk.Parse.Lexer where

import Control.Monad.Trans.State.Strict (State)
import Data.Bits (shiftR, (.&.))
import Data.Char (digitToInt, ord)
import Data.Text.Lazy (Text)
import Data.Word (Word8)
import Filesystem.Path.CurrentOS (FilePath)
import Lens.Micro.Mtl ((.=), (+=))
import Pipes (Producer, for, lift, yield)
import Prelude hiding (FilePath)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.Lazy                   as Text
import qualified Filesystem.Path.CurrentOS        as Filesystem

}


-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$digit = 0-9

$opchar = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

$fstLow       = [a-z]
$fstCap       = [A-Z]
$idchar = [A-Za-z0-9\_]

$nonwhite       = ~$white
$whiteNoNewline = $white # \n

-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

  -- Skip whitespace everywhere
  $whiteNoNewline                 ;
  \n                              { \_    -> lift (do
                                      line   += 1
                                      column .= 0 ) }
  "//".*                          ;
  
  $fstLow $idchar*                { \text -> yield (TokenVarId text) }
  $fstCap $idchar*                { \text -> yield (TokenConId text) }
  $opchar+                        { \text -> yield (TokenOpId text) }
  
  $digit+                         { \text -> yield (TokenInteger $ toInt text) }
  
  -- Need support for multi-line comments, chars, strings and floats.
  -- Note: It may be possible to parse char, strings, and floats with Earley.


{

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

-- | The cursor's location while lexing the text
data Position = P
    { lineNo    :: {-# UNPACK #-} !Int
    , columnNo  :: {-# UNPACK #-} !Int
    } deriving (Show)
    
defPos :: Position
defPos = P 1 0

-- line :: Lens' Position Int
line :: Functor f => (Int -> f Int) -> Position -> f Position
line k (P l c) = fmap (\l' -> P l' c) (k l)
-- column :: Lens' Position Int
column :: Functor f => (Int -> f Int) -> Position -> f Position
column k (P l c) = fmap (\c' -> P l c') (k c)

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
lexModl :: Text -> Producer Token (State Position) (Maybe Text)
lexModl text = for (go (AlexInput '\n' [] text)) tag
  where
    tag token = do
        pos <- lift State.get
        yield (Token token pos)

    go input = case alexScan input 0 of
        AlexEOF                        -> return Nothing
        AlexError (AlexInput _ _ text) -> return (Just text)
        AlexSkip  input' len           -> do
            lift (column += len)
            go input'
        AlexToken input' len act       -> do
            act (Text.take (fromIntegral len) (currInput input))
            lift (column += len)
            go input'

-- | A `Token` augmented with `Position` information
data Token = Token
    { token    ::                !TokenClass
    , position :: {-# UNPACK #-} !Position
    } deriving (Show)

-- The token type:
data TokenClass
  = TokenOpId Text
  | TokenVarId Text
  | TokenConId Text
  
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
  deriving ( Eq, Show )

}
