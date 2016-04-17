{
module Hawk.Grammar where

import Hawk.Tokens
}

%name parseHk
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEof }
%error { happyError }

%token
    ID      { Token _ (TokenId  $$) }
    INT     { Token _ (TokenInt  $$) }
    FLOAT   { Token _ (TokenFloat  $$) }
    CHAR    { Token _ (TokenChar  $$) }
    STRING  { Token _ (TokenString  $$) }
    
    VAL     { Token _ TokenVal }
    VAR     { Token _ TokenVar }
    
    DO      { Token _ TokenDo }
    RETURN  { Token _ TokenReturn }
    
    
    '::'    { Token _ TokenDblColon }
    
    ':='    { Token _ TokenFuncDef }
    ':-'    { Token _ TokenTypeDec }
    ':~'    { Token _ TokenTypeClass }
    ':+'    { Token _ TokenImplement }
    
    '<-'    { Token _ TokenLArrow }
    '<='    { Token _ TokenThickLArrow }
    '->'    { Token _ TokenRArrow }
    '=>'    { Token _ TokenThickRArrow }
    '<:'    { Token _ TokenSubtype }
    
    '`'     { Token _ TokenGrave }
    '~'     { Token _ TokenTilde }
    '!'     { Token _ TokenExclaim }
    '?'     { Token _ TokenQuestion }
    '@'     { Token _ TokenAt }
    '#'     { Token _ TokenPound }
    '$'     { Token _ TokenDollar }
    '%'     { Token _ TokenPercent }
    '^'     { Token _ TokenCaret }
    '&'     { Token _ TokenAmpersand }
    
    '('     { Token _ TokenLParen }
    ')'     { Token _ TokenRParen }
    '['     { Token _ TokenLBracket }
    ']'     { Token _ TokenRBracket }
    '{'     { Token _ TokenLCurlyBrace }
    '}'     { Token _ TokenRCurlyBrace }
    '|'     { Token _ TokenBar }
    
    ':'     { Token _ TokenColon }
    ';'     { Token _ TokenSemicolon }
    '.'     { Token _ TokenPeriod }
    ','     { Token _ TokenComma }
    '<'     { Token _ TokenLesser }
    '>'     { Token _ TokenGreater }
    
    '*'     { Token _ TokenStar }
    '/'     { Token _ TokenSlash }
    '+'     { Token _ TokenPlus }
    '-'     { Token _ TokenMinus }
    '='     { Token _ TokenEquals }
    
    OPEN_BLOCK    { Token _ TokenOpenBlock }
    CLOSE_BLOCK   { Token _ TokenCloseBlock }
    OPEN_STMT     { Token _ TokenOpenStmt }
    CLOSE_STMT    { Token _ TokenCloseStmt }

%%

ast :: { Expr }
  : mod_stmts { ModuleExpr [] $1 }


-- -----------------------------------------------------------------------------
-- Hawk Parser "General"  

ids :: { [String] }
  : ID            { [$1] }
  | ids ID        { $2 : $1 }
  
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Module and Import"

mod_stmt :: { Expr }
  : mod_dec       { $1 }
  | import        { $1 }
  | func_dec      { $1 }
  | elem          { $1 }
  
mod_stmts :: { [Expr] }
  : '{' mod_stmt '}'                { [$2] }
  | mod_stmts '{' mod_stmt '}'      { $3 : $1 }
  
mod_stmt_block :: { [Expr] }
  : '{' '}'                     { [] }
  | '{' mod_stmts '}'           { $2 }

mod_id :: { [String] }
  : ID                          { [$1] }
  | mod_id '.' ID               { $3 : $1 }

mod_dec :: { Expr }
  : mod_id '::' mod_stmt_block  { ModuleExpr $1 $3 }
  
import :: { Expr }
  : '->' mod_id              { ImportExpr $2 False }
  | '=>' mod_id              { ImportExpr $2 True  }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Expressions and statements"

expr :: { Expr }
  : INT                     { IntExpr $1 }
  | STRING                  { StringExpr $1 }
  | ID                      { VarExpr $1 }
  
  | DO stmt_block           { DoExpr $2 }
  | RETURN expr             { ReturnExpr $2 }
  
  | expr '+' expr           { PlusExpr $1 $3 }
  | expr '-' expr           { MinusExpr $1 $3 }
  | expr '*' expr           { TimesExpr $1 $3 }
  | expr '/' expr           { DivExpr $1 $3 }
  | '(' expr ')'            { $2 }
  

exprs :: { [Expr] }
  : expr              { [$1] }
  | exprs expr        { $2 : $1 }
  
stmt :: { Expr }
  : expr    { $1 }
  | elem    { $1 }
  
stmts :: { [Expr] }
  : '{' stmt '}'            { [$2] }
  | stmts '{' stmt '}'      { $3 : $1 }

stmt_block :: { [Expr] }
  : '{' '}'             { [] }
  | '{' stmts '}'       { $2 }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Function"
  
func_dec :: { Expr }
  : ID params ':' typesig '=' expr        { FuncDecExpr $1 $2 $4 $6 }
  | ID params ':' typesig ':=' stmt_block { FuncDecExpr $1 $2 $4 (DoExpr $6) }

params :: { [String] }
  : {- empty -}    { [] }
  | ids            { $1 }
  
typesig :: { [String] }
  : ID                  { [$1] }
  | typesig '->' ID     { $3:$1 }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Varibles and Values"

elem :: { Expr }
  : val { $1 }
  | var { $1 }

val :: { Expr }
  : VAL ID ':' typesig '=' expr { ValDecExpr $2 $4 $6 }
  
var :: { Expr }
  : VAR ID ':' typesig '=' expr { VarDecExpr $2 $4 $6 }

{

type Name = String
type Params = [String]
type TypeSig = [String]

data Expr
  = IntExpr Int
  | FloatExpr Double
  | StringExpr String
  | VarExpr String
  
  | ModuleExpr [String] [Expr]
  | ImportExpr [String] Bool
  
  | VarDecExpr Name TypeSig Expr
  | ValDecExpr Name TypeSig Expr
  
  | FuncDecExpr Name Params TypeSig Expr
  | ExternExpr Name Params TypeSig
  
  | LetExpr Name Expr Expr
  | CallExpr Name [Expr]
  
  | DoExpr [Expr]
  | ReturnExpr Expr
  | IfExpr Expr Expr Expr
  | WhileExpr Expr Expr
  
  | PlusExpr     Expr Expr
  | MinusExpr    Expr Expr
  | TimesExpr    Expr Expr
  | DivExpr      Expr Expr
  | NegateExpr   Expr
  deriving (Eq, Show)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError tok@(Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'" ++ "\n" ++ show tok)

parse :: FilePath -> String -> Either String Expr
parse = runAlex' parseHk

parseFile :: FilePath -> IO (Either String Expr)
parseFile p = readFile p >>= return . parse p


}
