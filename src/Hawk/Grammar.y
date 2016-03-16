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
    import { Token _ TokenImport }
    export { Token _ TokenExport }
    modid { Token _ (TokenModId $$) }
    varid { Token _ (TokenVarid $$) }
    varidp { Token _ (TokenVaridP $$) }
    vardef { Token _ TokenVarDef }
    int { Token _ (TokenInt $$) }
    prim_str { Token _ (TokenString $$) }
    var { Token _ (TokenSym $$) }
    '@' { Token _ TokenOption }
    '=' { Token _ TokenEq }
    '>' { Token _ TokenGT }
    '<' { Token _ TokenLT }
    '+' { Token _ TokenPlus }
    '-' { Token _ TokenMinus }
    '*' { Token _ TokenTimes }
    '/' { Token _ TokenDiv }
    '(' { Token _ TokenLParen }
    ')' { Token _ TokenRParen }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%
ast :: { Ast }
  : tlf                     { [$1] }
  | ast tlf                 { $2 : $1 }


tlf :: { TopLevelForm }
  : export ModuleList       { TLExport $2 }
  | import ModuleList       { TLImport $2 }
  | ExpDef                  { TLExpDef $1 }

ModuleList :: { [String] }
  : modid                   { [$1] }
  | ModuleList modid        { $2 : $1 }

ExpDef :: { ExpDef }
  : varid vardef Exp        { ExpDef $1 [] $3 }
  | varid params vardef Exp { ExpDef $1 $2 $4 }

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

type Ast = [TopLevelForm]

data TopLevelForm
  = TLExport [String]
  | TLImport [String]
  | TLOptDef [String]
  | TLTypDef
  | TLExpDef ExpDef
  | TLEmpty
  deriving(Eq,Show)


data ExpDef = ExpDef String [String] Exp
  deriving (Eq,Show)

data Exp
  = Exp String Exp
   | Plus Exp Exp
   | Minus Exp Exp
   | Times Exp Exp
   | Div Exp Exp
   | Negate Exp
   | Brack Exp
   | Int Int
   | PrimStr String
   | Var String
   deriving (Eq,Show)
   
data Primitive
  = PrimInt Integer
  | PrimDecimal Float
  | PrimString String
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