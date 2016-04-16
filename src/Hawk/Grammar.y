{
module Hawk.Grammar where

import Hawk.Tokens
}

%name parseSrc
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEof }
%error { happyError }

%token
    ID      { Token _ (TokenId  $$) }
    INT     { Token _ (TokenInteger  $$) }
    DECIMAL { Token _ (TokenDecimal  $$) }
    CHAR    { Token _ (TokenChar  $$) }
    STRING  { Token _ (TokenString  $$) }
    
    '::'    { Token _ TokenDblColon }
    
    ':='    { Token _ TokenFuncDec }
    ':-'    { Token _ TokenTypeDec }
    ':~'    { Token _ TokenTypeClass }
    ':+'    { Token _ TokenImplement }
    
    '<-'    { Token _ TokenLArrow }
    '<='    { Token _ TokenLLArrow }
    '->'    { Token _ TokenRArrow }
    '=>'    { Token _ TokenRRArrow }
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
    
    '{'     { Token _ TokenLParen }
    '}'     { Token _ TokenRParen }
    '['     { Token _ TokenLBracket }
    ']'     { Token _ TokenRBracket }
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
ast :: { Ast }
  : tl_smts { $1 }


tl_smt :: { TopLevelStmt }
  : ModStmt                 { $1 }
  | ImportStmt              { TLImport $2 }
  | ExpDef                  { TLExpDef $1 }
  
tl_smts :: { [TopLevelStmt] }
  : tl_smt                  { [$1] }
  | tl_smts tl_smt          { $2 : $1 }

id :: { String }
  : ID  { $1 }

ids :: { [String] }
  : id      { [$1] }
  | ids id  { $2 : $1 }

ModId :: { [String] }
  : ID                      { [$1] }
  | ModId "." ID            { $2 : $1 }

ModStmt :: { TopLevelStmt }
  : ModId "::" tl_smts      { TLModule $1 $3 }

ImportStmt :: { TopLevelStmt }
  : "->" ModId              { TLImport  $2 }
  | "=>" ModId              { TLImportQ $2 }
  
Func :: { HkFun }
  : ID 

params :: { [String] }
  : varidp                   { [$1]}
  | params varidp            { $2:$1 }

Exp :: { Exp }
  : var '=' Exp             { Exp $1 $3 }
  | Exp '+' Exp             { Plus $1 $3 }
  | Exp '-' Exp             { Minus $1 $3 }
  | Exp '*' Exp             { Times $1 $3 }
  | Exp '/' Exp             { Div $1 $3 }
  | '(' Exp ')'             { $2 }
  | '-' Exp %prec NEG       { Negate $2 }
  | int                     { Int $1 }
  | prim_str                { PrimStr $1 }
  | var                     { Var $1 }

{

type Ast = [TopLevelStmt]

data TopLevelStmt
  = TLModule  [String] [TopLevelStmt]
  | TLImport  [String]
  | TLImportQ [String]
  | TLFunction HkFun
  | TLVar HkVar
  | TLRec HkRec
  | TLEmpty
  deriving(Eq,Show)

data HkFun
  = HkFun {
    hkFunName       :: String
  , hkFunParams     :: [String]
  , hkFunTypeSig    :: HkTypeSig
  , hkFunExp        :: HkExp
  }

data HkExp
  =  HkExpPrim    HkPrim
   | HkExpVar     String HkExp
   | HkExpVal     String HkExp
   | HkExpDo      [HkExp]
   | HkExpPlus    HkExp HkExp
   | HkExpMinus   HkExp HkExp
   | HkExpTimes   HkExp HkExp
   | HkExpDiv     HkExp HkExp
   | HkExpNegate  HkExp
   deriving (Eq,Show)
   
data HkPrim
  = HkPrimInt Integer
  | HkPrimDecimal Float
  | HkPrimString String
  deriving (Eq,Show)
   
data Expr
  = Module String [Expr]
  | Submodule String [Expr]
  | Import String
  | IdRef String
  | Prim Primitive
  | FunctionCall String [Expr]
  | Assignment String Expr
  | Return Expr
  | Record String [Expr]
  | Variable String Expr
  | Function String [String] Expr
  deriving (Eq,Show)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError tok@(Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'" ++ "\n" ++ show tok)

parse :: FilePath -> String -> Either String Ast
parse = runAlex' parseSrc


}
