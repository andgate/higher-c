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
  ':'                { Token (TokenRsvp ":") _ $$ }
  ';'                { Token (TokenRsvp ";") _ $$ }
  ','                { Token (TokenRsvp ",") _ $$ }
  '='                { Token (TokenRsvp "=") _ $$ }
  lambda             { Token (TokenRsvp "λ") _ $$ }
  '.'                { Token (TokenRsvp ".") _ $$ }
  '_'                { Token (TokenRsvp "_") _ $$ }

  '('                { Token (TokenRsvp "(") _ $$ }
  ')'                { Token (TokenRsvp ")") _ $$ }
  '['                { Token (TokenRsvp "[") _ $$ }
  ']'                { Token (TokenRsvp "]") _ $$ }
  '{'                { Token (TokenRsvp "{") _ $$ }
  '}'                { Token (TokenRsvp "}") _ $$ }

  module             { Token (TokenRsvp "module") _ $$ }
  import             { Token (TokenRsvp "import") _ $$ }
  data               { Token (TokenRsvp "data"  ) _ $$ }

  return             { Token (TokenRsvp "return") _ $$ }
  let                { Token (TokenRsvp "let")  _ $$ }
  in                 { Token (TokenRsvp "in")   _ $$ }
  where              { Token (TokenRsvp "where") _ $$ }

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

ModDecl : module ModName TopLevelBlock { TMod $2 $3 }

ModName : conId        { mkQName (extractId $1) }
        | qconId       { mkQName (extractId $1) }


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

SDecl : VarName                  { SDecl $1 Nothing Nothing }
      | VarName ':' Type         { SDecl $1 Nothing Nothing }
      | VarName ':' Type '=' Exp { SDecl $1 (Just $3) (Just $5) }

SCall : Exp '(' ExpList0  ')' { SCall $1 $3 }

SReturn : return Exp { SReturn $2 }

ExpList0 : {- empty -} { [] }
         | ExpList     { $1 }

ExpList : Exp             { [$1] }
        | ExpList ',' Exp { $3 : $1 }

Exp : VarId { mkVar $1 }
    | ConId { mkCon $1 }
    | Value { mkVal $1 }

Type : VarId { mkTVar $1 }
     | ConId { mkTCon $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
