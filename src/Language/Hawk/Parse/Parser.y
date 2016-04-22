{
module Language.Hawk.Parse.Parser where

import Language.Hawk.Parse.Lexer
import Language.Hawk.Syntax.AST
import Language.Hawk.Data.Node

}

%name parseHk
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEof }
%error { happyError }

%token
    ID      { Token _ (TokenId  _) }
    INT     { Token _ (TokenInt  _) }
    FLOAT   { Token _ (TokenFloat  _) }
    CHAR    { Token _ (TokenChar  _) }
    STRING  { Token _ (TokenString  _) }
    
    EXTERN  { Token _ TokenExtern }
    
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

ids :: { Ids }
  : ID            { [getTokId $1] }
  | ids ID        { (getTokId $2) : $1 }
  
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Module and Import"

mod_stmt :: { Expr }
  : mod_dec       { $1 }
  | import        { $1 }
  | func          { $1 }
  | extern_func   { $1 }
  | elem          { $1 }
  
mod_stmts :: { Exprs }
  : '{' mod_stmt '}'                { [$2] }
  | mod_stmts '{' mod_stmt '}'      { $3:$1 }
  
mod_stmt_block :: { Exprs }
  : '{' '}'                     { [] }
  | '{' mod_stmts '}'           { reverse $2 }

mod_id :: { ModId }
  : ID                          { [getTokId $1] }
  | mod_id '.' ID               { (getTokId $3):$1 }

mod_dec :: { Expr }
  : mod_id '::' mod_stmt_block  { ModuleExpr (reverse $1) $3 }
  
import :: { Expr }
  : '->' mod_id              { ImportExpr $2 False }
  | '=>' mod_id              { ImportExpr $2 True  }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Expressions and statements"

expr :: { Expr }
  : INT                     { IntExpr (getTokInt $1) }
  | STRING                  { StringExpr (getTokString $1) }
  | ID                      { VarExpr (getTokId $1) }
  
  | DO stmt_block           { DoExpr $2 }
  | RETURN expr             { ReturnExpr $2 }
  
  | expr '+' expr           { BinaryOpExpr "+" $1 $3 }
  | expr '-' expr           { BinaryOpExpr "-" $1 $3 }
  | expr '*' expr           { BinaryOpExpr "*" $1 $3 }
  | expr '/' expr           { BinaryOpExpr "/" $1 $3 }
  | '(' expr ')'            { $2 }
  

exprs :: { Exprs }
  : expr              { [$1] }
  | exprs expr        { $2:$1 }
  
stmt :: { Expr }
  : expr    { $1 }
  | elem    { $1 }
  
stmts :: { Exprs }
  : '{' stmt '}'            { [$2] }
  | stmts '{' stmt '}'      { $3:$1 }

stmt_block :: { Exprs }
  : '{' '}'             { [] }
  | '{' stmts '}'       { reverse $2 }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Function"
  
func :: { Expr }
  : func_dec func_def { mkFuncExpr $1 $2 }
  
func_dec :: { (Name, Params, Types) }
  : ID params typesig { (getTokId $1, $2, $3) }
  
func_def :: { Expr }
  : '=' expr { $2 }
  | ':=' stmt_block { DoExpr $2 }

params :: { Params }
  : {- empty -}    { [] }
  | ids            { reverse $1 }
  
types :: { Types }
  : ID                { [getTokId $1] }
  | types '->' ID     { (getTokId $3) : $1 }
  
typesig :: { Types }
  : ':' types         { reverse $2 }

extern_func :: { Expr }
  : EXTERN func_dec { mkExternExpr $2 }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Variables and Values"

elem :: { Expr }
  : val { $1 }
  | var { $1 }

val :: { Expr }
  : VAL ID typesig '=' expr { ValDecExpr (getTokId $2) $3 $5 }
  
var :: { Expr }
  : VAR ID typesig '=' expr { VarDecExpr (getTokId $2) $3 $5 }

{


getTokId      (Token _ (TokenId s))     = s
getTokInt     (Token _ (TokenInt s))    = s
getTokString  (Token _ (TokenString s)) = s

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError tok@(Token (TokenInfo p n) t) =
  alexError' p ("parse error at token '" ++ show t ++ "'" ++ "\n" ++ show tok)

parse :: FilePath -> String -> Either String Expr
parse = runAlex' parseHk

parseFile :: FilePath -> IO (Either String Expr)
parseFile p = readFile p >>= return . parse p


}
