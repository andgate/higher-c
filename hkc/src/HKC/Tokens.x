{
module HKC.Tokens where

import Prelude hiding (lex)
import Control.Monad ( liftM )
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

-- 0 is the toplevel parser
<0> {
  "--".*                        ;
  ^\s*\<                        { lex' TokenExport `andBegin` modulelist }
  ^\s*\>                        { lex' TokenImport `andBegin` modulelist }
  ^\s*@varid                    { lex TokenVarid   `andBegin` startvar }
  .                             ;
}

<modulelist> {
  $white_no_nl+                 ;
  @modid                        { lex TokenModId }
  $white+                       { begin 0 }
}

<startvar> {
  $white+                       ;
  \=                            { lex' TokenVarDef `andBegin` statements }
  @varid                        { lex TokenVaridP }
}

<statements> {
  $white_no_nl+                 ;
  $digit+                       { lex (TokenInt . read) }
  @varid                        { lex TokenSym }
  \+                            { lex' TokenPlus }
  \-                            { lex' TokenMinus }
  \*                            { lex' TokenTimes }
  \/                            { lex' TokenDiv }
  $white+                       { begin 0 }
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
  \'                            { begin 0 }
}

<string> {
  [^\"]                         { lex TokenString }
  \"                            { pop }
}
{

data AlexUserState
  = AlexUserState
  { filePath    :: FilePath
  , scopes      :: [Int]
  , startCodes  :: [Int]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" [] []

alexGetFilePath :: Alex FilePath
alexGetFilePath = liftM filePath alexGetUserState

alexSetFilePath :: FilePath -> Alex ()
alexSetFilePath f = do
  s <- alexGetUserState
  alexSetUserState (s { filePath = f })

peekScope :: Alex Int
peekScope = liftM (head . scopes) alexGetUserState

pushScope :: Int -> Alex ()
pushScope s' = do
  aus <- alexGetUserState
  let s   = scopes aus
      s'' = s':s
  alexSetUserState (aus { scopes = s'' })

popScope :: Alex Int
popScope = do
  aus <- alexGetUserState
  let s  = scopes aus
      s' = tail s
  alexSetUserState (aus { scopes = s' })
  return $ head s

peekStartCode :: Alex Int
peekStartCode = liftM (head . startCodes) alexGetUserState

pushStartCode :: Int -> Alex ()
pushStartCode sc' = do
  aus <- alexGetUserState
  let sc  = startCodes aus
      sc'' = sc' : sc
  alexSetUserState (aus { startCodes = sc'' })

popStartCode :: Alex Int
popStartCode = do
  aus <- alexGetUserState
  let sc  = startCodes aus
      sc' = tail sc
  alexSetUserState (aus { startCodes = sc' })
  return (head sc)

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
  alexSetStartCode code
  pushStartCode code
  alexMonadScan'

exitStartCode :: Action
exitStartCode input len = do
  popStartCode
  sc <- alexPeekStartCode
  alexSetStartCode sc
  alexMonadScan'

enterBOL :: Int -> AlexAction Token
enterBOL s' input len = do
  s <- peekScope
  pushScope s'
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
  fp <- getFilePath
  alexError (fp ++ ": " ++ show l ++ ": " ++ show c ++ ": " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}
