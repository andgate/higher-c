{

{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Parse where

import Language.Hawk.Lex
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Concrete
import Language.Hawk.Syntax.Builtin
import Language.Hawk.Syntax.Location

}

%name parseHk Src
%tokentype { Token }
%error { parseError }


%token
  backslash          { Token (TokenRsvp "\\") _ $$ }
  rarrow             { Token (TokenRsvp "->") _ $$ }
  ':'                { Token (TokenRsvp ":") _ $$ }
  '::'               { Token (TokenRsvp "::") _ $$ }
  ';'                { Token (TokenRsvp ";") _ $$ }
  ','                { Token (TokenRsvp ",") _ $$ }
  '.'                { Token (TokenRsvp ".") _ $$ }
  '='                { Token (TokenRsvp "=") _ $$ }
  '_'                { Token (TokenRsvp "_") _ $$ }
  '~'                { Token (TokenRsvp "~") _ $$ }
  '*'                { Token (TokenRsvp "*") _ $$ }
  '&'                { Token (TokenRsvp "&") _ $$ }

  '('                { Token (TokenRsvp "(") _ $$ }
  ')'                { Token (TokenRsvp ")") _ $$ }
  '['                { Token (TokenRsvp "[") _ $$ }
  ']'                { Token (TokenRsvp "]") _ $$ }
  '{'                { Token (TokenRsvp "{") _ $$ }
  '}'                { Token (TokenRsvp "}") _ $$ }
  '<'                { Token (TokenRsvp "<") _ $$ }
  '>'                { Token (TokenRsvp ">") _ $$ }

  Type               { Token (TokenRsvp "Type") _ $$ }
  Void               { Token (TokenRsvp "Void") _ $$ }
  I32                { Token (TokenRsvp "I32" ) _ $$ }

  let                { Token (TokenRsvp "let"   ) _ $$ }
  static             { Token (TokenRsvp "static") _ $$ }
  inline             { Token (TokenRsvp "inline") _ $$ }
  const              { Token (TokenRsvp "const" ) _ $$ }
  as                 { Token (TokenRsvp "as"    ) _ $$ }

  new                { Token (TokenRsvp "new"  ) _ $$ }
  newer              { Token (TokenRsvp "newer") _ $$ }
  delete             { Token (TokenRsvp "delete) _ $$ }

  module             { Token (TokenRsvp "module") _ $$ }
  import             { Token (TokenRsvp "import") _ $$ }

  type               { Token (TokenRsvp "type" ) _ $$ }
  class              { Token (TokenRsvp "class") _ $$ }
  impl               { Token (TokenRsvp "impl" ) _ $$ }

  if                 { Token (TokenRsvp "if"  ) _ $$ }
  else               { Token (TokenRsvp "else") _ $$ }
  elif               { Token (TokenRsvp "elif") _ $$ }
  case               { Token (TokenRsvp "case") _ $$ }

  try                { Token (TokenRsvp "try"    ) _ $$ }
  catch              { Token (TokenRsvp "catch"  ) _ $$ }
  finally            { Token (TokenRsvp "finally") _ $$ }

  return             { Token (TokenRsvp "return") _ $$ }
  break              { Token (TokenRsvp "break" ) _ $$ }
  continue           { Token (TokenRsvp "continue") _ $$ }

  do                 { Token (TokenRsvp "do") _ $$ }
  while              { Token (TokenRsvp "while") _ $$ }
  for                { Token (TokenRsvp "for") _ $$ }


  varId              { Token (TokenVarId  _) _ _ }
  conId              { Token (TokenConId  _) _ _ }
  qvarId             { Token (TokenQVarId _) _ _ }
  qconId             { Token (TokenQConId _) _ _ }

  primAdd            { Token (TokenPrimId "#add") _ $$ }
  primSub            { Token (TokenPrimId "#sub") _ $$ }

  integer            { Token (TokenInteger _) _ _ }
  double             { Token (TokenDouble  _) _ _ }
  char               { Token (TokenChar    _) _ _ }
  string             { Token (TokenString  _) _ _ }
  boolean            { Token (TokenBool    _) _ _ }

%%

Src : TopLevels { Src "" $1 }

TopLevelBlock : '{' '}'           { [] }
              | '{' TopLevels '}' { $2 }

TopLevels : TopLevel { [$1] }
          | TopLevels TopLevel { $2 : $1 }

TopLevel : ModDecl { $1 }
         | Func { TFunc $1 }
         | VarDecl {

ModDecl : module ModName TopLevelBlock { TMod $2 $3 }

ModName : conId        { mkQName (extractId $1) }
        | qconId       { mkQName (extractId $1) }

VarDecl : SDecl

VarId : varId { extractId $1 }
ConId : conId { extractId $1 }

VarName : VarId { mkName $1 }
ConName : ConId { mkName $1 }

Name : VarName { $1 }
     | ConName { $1 }


Value : integer  { fmap VInt    (extractInteger $1) }
      | double   { fmap VFloat  (extractDouble  $1) }
      | char     { fmap VChar   (extractChar    $1) }
      | string   { fmap VString (extractString  $1) }
      | boolean  { fmap VBool   (extractBool    $1) }




Import : import ModName { TImport $2 }


Func : FuncDecl Block { Func $1 $2 }

FuncDecl : VarName '(' Args0 ')'          { FuncDecl $1 $3 Nothing   }
         | VarName '(' Args0 ')' ':' Type { FuncDecl $1 $3 (Just $6) }

Args0 : {- empty -} { Args [] }
      | Args        { Args $1 }

Args : Arg            { [$1] }
     | Args ',' Arg   { $3 : $1 }

Arg : VarName          { Arg $1 Nothing   }
    | VarName ':' Type { Arg $1 (Just $3) }


Block : '{' '}'       { Block [] }
      | '{' Stmts '}' { Block $2 }


Stmts : Stmt       { [$1] }
      | Stmts Stmt { $2 : $1 }

Stmt : AStmt ';' { $1 }

AStmt : SCall { $1 }
      | SDecl { $1 }

SDecl : let VarName                  { SDecl $2 Nothing   Nothing }
      | let VarName ':' Type         { SDecl $2 (Just $4) Nothing }
      | let VarName ':' Type '=' Exp { SDecl $2 (Just $4) (Just $6) }

SCall : Exp '(' ExpList0  ')' { SCall $1 $3 }

SReturn : return Exp { SReturn $2 }

ExpList0 : {- empty -} { [] }
         | ExpList     { $1 }

ExpList : Exp             { [$1] }
        | ExpList ',' Exp { $3 : $1 }

Exp : VarId { mkVar $1 }
    | ConId { mkCon $1 }
    | Value { mkVal $1 }

Type : Type2 { $1 }

Type2 : '*' Type1 { }
      | '&' Type1 {}

Type1 : VarId { mkTVar $1 }
      | ConId { mkTCon $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
