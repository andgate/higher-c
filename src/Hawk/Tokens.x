{
module Hawk.Tokens where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import qualified Data.ByteString.Lazy as B

}

%wrapper "monadUserState"

-- A lot of this is based on GHC's Lexer.
-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

-- NB: The logic behind these definitions is also reflected in basicTypes/Lexeme.hs
-- Any changes here should likely be reflected there.
$unispace    = \x05 -- Trick Alex into handling Unicode. See alexGetByte.
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n -- TODO #8424
$tab         = \t


$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See alexGetByte.
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See alexGetByte.
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']
$unilarge  = \x01 -- Trick Alex into handling Unicode. See alexGetByte.
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]
$unismall  = \x02 -- Trick Alex into handling Unicode. See alexGetByte.
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]
$unigraphic = \x06 -- Trick Alex into handling Unicode. See alexGetByte.
$graphic   = [$small $large $symbol $digit $special $unigraphic \"\']
$binit     = 0-1
$octit     = 0-7
$hexit     = [$decdigit A-F a-f]
$suffix    = \x07 -- Trick Alex into handling Unicode. See alexGetByte.
-- TODO #10196. Only allow modifier letters in the suffix of an identifier.
$idchar    = [$small $large $digit $suffix \']
$pragmachar = [$small $large $digit]
$docsym    = [\| \^ \* \$]

-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@varid     = $small $idchar*          -- variable identifiers
@typid     = $large $idchar*          -- constructor identifiers
@modid     = $idchar+ (\. $idchar+)*
@varsym    = ($symbol # \:) $symbol*  -- variable (operator) symbol
@consym    = \: $symbol*              -- constructor (operator) symbol
@decimal     = $decdigit+
@binary      = $binit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal
@qual = (@typid \.)+
@qvarid = @qual @varid
@qtyid = @qual @typid
@qvarsym = @qual @varsym
@qconsym = @qual @consym
@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent
-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-
@signed = @negative ?

-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

-- Skip whitespace everywhere
$white_no_nl+                   ;
"--".*                          ;


-- 0 is the toplevel parser
<0> {
  \<                            { lex' TokenExport `andEnter` modulelist }
  \>                            { lex' TokenImport `andEnter` modulelist }
  @varid                        { lex  TokenVarid  `andEnter` start_var }
  @typid                        { lex  TokenTypid  `andEnter` start_type }
}

<modulelist> {
  \x0A $white*                  { enterBOL }
  @modid                        { lex TokenModId }
}

<start_var> {
  \@                            ;
  \:                            ;
  \=                            { exitAfter (lex' TokenVarDef `andEnter` statements) }
  @varid                        { lex TokenVaridP }
}

<start_type> {
  \@                            ;
  \:                            ;
  \=                            { exitAfter (lex' TokenVarDef `andEnter` statements) }
  @typid                       { lex TokenVaridP }
}

<statements> {
  $white_no_nl+                 ;
  $digit+                       { lex (TokenInt . read) }
  @varid                        { lex TokenSym }
  \+                            { lex' TokenPlus }
  \-                            { lex' TokenMinus }
  \*                            { lex' TokenTimes }
  \/                            { lex' TokenDiv }
  $white+                       { exitStartCode }
  \"                            { enterStartCode string }
}

<old> {
  $white+                       ;
  $digit+                       { lex (TokenInt . read) }
  ^\<                           { lex' TokenExport }
  ^\>                           { lex' TokenImport }
  \<                            { lex' TokenGT }
  \>                            { lex' TokenLT }
  \@                            { lex' TokenOption }
  \-\>                          { lex' TokenTo }
  \:                            { lex' TokenType }
  \=                            { lex' TokenEq }
  \+                            { lex' TokenPlus }
  \-                            { lex' TokenMinus }
  \*                            { lex' TokenTimes }
  \/                            { lex' TokenDiv }
  \(                            { lex' TokenLParen }
  \)                            { lex' TokenRParen }
  @varid                        { lex TokenSym }
  \"                            { begin string }
  \'                            { begin char }
}

<char> {
  [^\']                         { lex TokenChar }
  \'                            { exitStartCode }
}

<string> {
  [^\"]                         { lex TokenString }
  \"                            { exitStartCode }
}


{

data AlexUserState
  = AlexUserState
  { filePath    :: FilePath
  , scopes      :: [Int]
  , startCodes  :: [Int]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" [0] [0]

alexGetFilePath :: Alex FilePath
alexGetFilePath = liftM filePath alexGetUserState

alexSetFilePath :: FilePath -> Alex ()
alexSetFilePath f = do
  aus <- alexGetUserState
  alexSetUserState (aus { filePath = f } )


alexPeekScope :: Alex Int
alexPeekScope = liftM (head . scopes) alexGetUserState

alexPushScope :: Int -> Alex ()
alexPushScope s = do
  aus <- alexGetUserState
  let ss = scopes aus
      ss' = s : ss
  alexSetUserState (aus { scopes = ss' })

alexPopScope :: Alex Int
alexPopScope = do
  aus <- alexGetUserState
  let ss  = scopes aus
      ss' = tail ss
      s   = headOrZero ss
      --s'  = headOrZero ss'
  alexSetUserState (aus { scopes = ss' })
  return s

alexUpdateScope :: Int -> Alex ()
alexUpdateScope s' = do
  s <- alexPeekScope
  if s' > s
    then alexPushScope s'
    else if s' < s
      then do alexPopScope
              -- If new scope is less, pop current startcode
              alexPopStartCode
              alexUpdateScope s'
      else return ()


alexPeekStartCode :: Alex Int
alexPeekStartCode = liftM (head . startCodes) alexGetUserState

alexPushStartCode :: Int -> Alex ()
alexPushStartCode sc = do
  aus <- alexGetUserState
  let scs  = startCodes aus
      scs' = sc : scs
  alexSetUserState (aus { startCodes = scs' })
  alexSetStartCode sc

alexPopStartCode :: Alex Int
alexPopStartCode = do
  aus <- alexGetUserState
  let scs  = startCodes aus
      scs' = tail scs
      sc   = headOrZero scs
      sc'  = headOrZero scs'
  alexSetUserState (aus { startCodes = scs' })
  alexSetStartCode sc'
  return sc

headOrZero :: [Int] -> Int
headOrZero (x:_) = x
headOrZero [] = 0

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum


data Token = Token AlexPosn TokenClass
  deriving (Eq, Show)

-- The token type:
data TokenClass = TokenExport
           | TokenImport
           | TokenModId String
           | TokenVarid String
           | TokenVaridP String
           | TokenVarDef
           | TokenTypid String
           | TokenOption
           | TokenType
           | TokenTo
           | TokenInt Int
           | TokenChar String
           | TokenString String
           | TokenSym String
           | TokenEq
           | TokenGT
           | TokenLT
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenEof
           deriving ( Eq, Show )

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEof

lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

lex' :: TokenClass -> AlexAction Token
lex' = lex . const

enterStartCode :: Int -> AlexAction Token
enterStartCode code input len = do
  alexPushStartCode code
  alexMonadScan'

exitStartCode :: AlexAction Token
exitStartCode input len = do
  alexPopStartCode
  alexMonadScan'

andEnter :: AlexAction Token -> Int -> AlexAction Token
andEnter act code input len = do
  alexPushStartCode code
  act input len

exitAfter :: AlexAction Token -> AlexAction Token
exitAfter act input len = do
  alexPopStartCode
  act input len

enterBOL :: AlexAction Token
enterBOL (_,_,_,istr) len = do
  let s' = length $ take len istr
  alexUpdateScope s'
  alexMonadScan'


alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
      alexError' p ("lexical error at chracter '" ++ take 1 s ++ "'")
    AlexSkip inp' len -> do
      alexSetInput inp'
      alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- alexGetFilePath
  alexError (fp ++ ": " ++ show l ++ ": " ++ show c ++ ": " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (alexSetFilePath fp >> a)

}
