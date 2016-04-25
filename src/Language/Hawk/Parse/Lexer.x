{
module Language.Hawk.Parse.Lexer where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import qualified Data.ByteString.Lazy as B

import Language.Hawk.Data.Node

}

%wrapper "monadUserState"


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
$small     = [$ascsmall $unismall]

$unigraphic = \x06 -- Trick Alex into handling Unicode. See alexGetByte.
$graphic   = [$small $large $symbol $digit $special $unigraphic \"\']

-- TODO #10196. Only allow modifier letters in the suffix of an identifier.
$idchar    = [$small $large]

-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@id_lower               = $small+
@id_cap_uscore          = $large [$idchar\_]*
@id_uscore_num_tick     = $small [$idchar\_$digit]*  \`?
@id_cap_uscore_num_tick = $large [$idchar\_$digit]*  \`?

@negative           = \-
@decimal            = $digit+
@integer            = @negative? @decimal
@exponent           = [eE] [\-\+]? @decimal
@floating_point     = @decimal \. @decimal @exponent? | @decimal @exponent

-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

-- Skip whitespace everywhere
$white+                   ;
"--".*                          ;


-- 0 is the toplevel parser
<0> {
  "mod"                         { lex' TokenModule }
  "use"                         { lex' TokenUse }
  "use!"                        { lex' TokenUseQualified }
  "as"                          { lex' TokenAs }
    
  "pub"                         { lex' TokenPublic }
  "priv"                        { lex' TokenPrivate }
  "link"                        { lex' TokenLink }
    
  "fn"                          { lex' TokenFunction }
  "val"                         { lex' TokenValue }
  "var"                         { lex' TokenVariable }
  
  "do"                          { lex' TokenDo }
  "return"                      { lex' TokenReturn }
  
  "if"                          { lex' TokenIf }
  "then"                        { lex' TokenThen }
  "else"                        { lex' TokenElse }
  "elif"                        { lex' TokenElif }
  "while"                       { lex' TokenWhile }
  
  \(\)                          { lex' TokenParenPair }
  "Bit"                         { lex' TokenBitTy }
  "W8"                          { lex' TokenW8Ty }
  "W16"                         { lex' TokenW16Ty }
  "W32"                         { lex' TokenW32Ty }
  "W64"                         { lex' TokenW64Ty }
  "I8"                          { lex' TokenI8Ty }
  "I16"                         { lex' TokenI16Ty }
  "I32"                         { lex' TokenI32Ty }
  "I64"                         { lex' TokenI64Ty }
  "F32"                         { lex' TokenF32Ty }
  "F64"                         { lex' TokenF64Ty }
  "Char"                        { lex' TokenCharTy }
  
  @id_lower                     { lex TokenIdLower }
  @id_cap_uscore                { lex TokenIdCapUscore }
  @id_uscore_num_tick           { lex TokenIdUScoreNumTick }
  @id_cap_uscore_num_tick       { lex TokenIdCapUScoreNumTick }
  
  @integer                      { lex (TokenInt . read) }
  
  
  \:                            { lex' TokenColon }
  \:\:                          { lex' TokenDblColon }
  
  \|                            { lex' TokenBar }
  \*                            { lex' TokenStar }
  \_                            { lex' TokenUnderscore }
  
  \:\=                          { lex' TokenFuncDef }
  \=                            { lex' TokenEquals }
  
  \<\-                          { lex' TokenRArrow }
  \<\=                          { lex' TokenThickRArrow }
  \-\>                          { lex' TokenRArrow }
  \=\>                          { lex' TokenThickRArrow }
  \<\:                          { lex' TokenSubtype }
  
  
  \(                            { lex' TokenLParen }
  \)                            { lex' TokenRParen }
  \{                            { lex' TokenLCurlyBrace }
  \}                            { lex' TokenRCurlyBrace }
  
  \.                            { lex' TokenPeriod }
  \,                            { lex' TokenComma }
  \<                            { lex' TokenGreater }
  \>                            { lex' TokenLesser }
  \+                            { lex' TokenPlus }
  \-                            { lex' TokenMinus }
  \/                            { lex' TokenSlash }
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

data Token = Token TokenInfo TokenClass
  deriving (Eq, Show)
  
data TokenInfo
  = TokenInfo 
  { tokiFilename :: String
  , tokiPos      :: AlexPosn
  , tokiLength   :: Int
  }
  deriving (Eq, Show)
  
instance HkNode TokenInfo where
  nodeInfo toki@(TokenInfo n _ _) = NodeInfo n (spanOf toki)
  
instance HkSpan TokenInfo where
  spanOf (TokenInfo _ (AlexPn _ lineNum colNum) l)
    = Span lineNum lineNum colNum (colNum + l)

instance HkNode Token where
  nodeInfo (Token toki _) = nodeInfo toki
  
instance HkSpan Token where
  spanOf (Token toki _) = spanOf toki

-- The token type:
data TokenClass
  = TokenIdLower String
  | TokenIdCapUscore String
  | TokenIdUScoreNumTick String
  | TokenIdCapUScoreNumTick String
  
  | TokenInt Int
  | TokenFloat Float
  | TokenChar String
  | TokenString String
  
  | TokenModule
  | TokenUse
  | TokenUseQualified
  | TokenAs
    
  | TokenPublic
  | TokenPrivate
  | TokenLink
    
  | TokenFunction
  | TokenValue
  | TokenVariable
  
  | TokenDo
  | TokenReturn
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenElif
  | TokenWhile
  
  | TokenParenPair
  | TokenBitTy
  | TokenW8Ty
  | TokenW16Ty
  | TokenW32Ty
  | TokenW64Ty
  | TokenI8Ty
  | TokenI16Ty
  | TokenI32Ty
  | TokenI64Ty
  | TokenF32Ty
  | TokenF64Ty
  | TokenCharTy
  
  | TokenDblColon
  
  | TokenFuncDef
  | TokenTypeDec
  | TokenTypeClass
  | TokenImplement
  
  | TokenLArrow
  | TokenThickLArrow
  | TokenRArrow
  | TokenThickRArrow
  | TokenSubtype
  
  | TokenGrave
  | TokenTilde
  | TokenExclaim
  | TokenQuestion
  | TokenAt
  | TokenPound
  | TokenDollar
  | TokenPercent
  | TokenCaret
  | TokenAmpersand
  
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenLCurlyBrace
  | TokenRCurlyBrace
  | TokenBar
  
  | TokenUnderscore
  | TokenColon
  | TokenSemicolon
  | TokenPeriod
  | TokenComma
  | TokenLesser
  | TokenGreater
  | TokenStar
  | TokenSlash
  | TokenPlus
  | TokenMinus
  | TokenEquals
  | TokenOpenBlock
  | TokenCloseBlock
  | TokenOpenStmt
  | TokenCloseStmt
  
  | TokenEof
  deriving ( Eq, Show )


tokPos :: Token -> AlexPosn
tokPos (Token (TokenInfo _ p _) _) = p

tokLineNum :: Token -> Int
tokLineNum = getLineNum . tokPos

tokColumnNum :: Token -> Int
tokColumnNum = getColumnNum . tokPos

tokFilePath :: Token -> String
tokFilePath (Token (TokenInfo n _ _) _) = n

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  n <- alexGetFilePath
  return $ Token (TokenInfo n p 0) TokenEof

lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> do
  n <- alexGetFilePath
  return $ Token (TokenInfo n p i) (f (take i s))

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
