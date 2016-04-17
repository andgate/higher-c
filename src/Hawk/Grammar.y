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
    
    DO      { Token _ TokenDo }
    
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
  : tl_stmts { ModuleExpr [] $1 }

tl_stmt :: { Expr }
  : mod_dec       { $1 }
  | import        { $1 }
  | func_dec      { $1 }
  
tl_stmts :: { [Expr] }
  : tl_stmt                 { [$1] }
  | tl_stmts tl_stmt        { $2 : $1 }
  

id :: { String }
  : ID  { $1 }

ids :: { [String] }
  : {- empty -}   { [] }
  | id            { [$1] }
  | ids id        { $2 : $1 }

mod_dec :: { Expr }
  : mod_id '::' '{' '}'               { ModuleExpr $1 [] }
  | mod_id '::' '{' tl_stmts '}'      { ModuleExpr $1 $4 }

mod_id :: { [String] }
  : ID                      { [$1] }
  | mod_id '.' ID            { $3 : $1 }

import :: { Expr }
  : '->' mod_id              { ImportExpr $2 False }
  | '=>' mod_id              { ImportExpr $2 True  }
  
func_dec :: { Expr }
  : id params ':' typesig ':=' expr   { FuncExpr $1 $2 $4 $6 }

params :: { [String] }
  : ID                   { [$1]}
  | params ID            { $2:$1 }
  
typesig :: { [String] }
  : ID                  { [$1] }
  | typesig '->' ID     { $3:$1 }

expr :: { Expr }
  : DO exprs                { DoExpr $2 }
  | expr '+' expr           { PlusExpr $1 $3 }
  | expr '-' expr           { MinusExpr $1 $3 }
  | expr '*' expr           { TimesExpr $1 $3 }
  | expr '/' expr           { DivExpr $1 $3 }
  | '(' expr ')'            { $2 }
  | INT                     { IntExpr $1 }
  | STRING                  { StringExpr $1 }
  | ID                      { VarExpr $1 }

exprs :: { [Expr] }
  : expr              { [$1] }
  | exprs expr        { $2 : $1 }
  
stmt :: { Expr }
  : '{' expr '}'      { $2 }

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
  
  | FuncExpr Name Params TypeSig Expr
  | ExternExpr Name Params TypeSig
  
  | LetExpr Name Expr Expr
  | CallExpr Name [Expr]
  
  | DoExpr [Expr]
  | Return Expr
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
