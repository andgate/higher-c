{

{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Parse where

import Language.Hawk.Lex
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Concrete
import Language.Hawk.Syntax.Builtin
import Language.Hawk.Syntax.Location

}

%name hawkFile TopLevels
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

TopLevelBlock : '{' '}'           { [] }
              | '{' TopLevels '}' { $1 }

TopLevels : TopLevel { [$1] }
          | TopLevels TopLevel { $2 : $1 }

TopLevel : ModDecl { $1 }
         | Func { TFunc $1 }

ModDecl : module ModName '{' TopLevelBlock '}' { ModDecl $2 $4 }

ModName : conId        { mkQName (extractId $1) }
        | qconId       { mkQName (extractId $1) }



Value : integer  { fmap VInt    (extractInteger $1) }
      | double   { fmap VFloat  (extractDouble  $1) }
      | char     { fmap VChar   (extractChar    $1) }
      | string   { fmap VString (extractString  $1) }
      | boolean  { fmap VBool   (extractBool    $1) }




Import : import ModName { $2 }


Func : FuncDecl Block { Func $1 $2 }

FuncDecl : varId '(' Args0 ')'          { FuncDecl $1 $3 Nothing   }
         | varId '(' Args0 ')' ':' Type { FuncDecl $1 $3 (Just $6) }

Args0 : {- empty -} { [] }
      | Args        { $1 }

Args : Arg            { [$1] }
     | Args ',' Arg { $3 : $1 }

Arg : varId          { Arg $1 Nothing   }
    | varId ':' Type { Arg $1 (Just $3) }


Block : '{' '}'       { [] }
      | '{' Stmts '}' { $2 }


Stmts : Stmt       { [$1] }
      | Stmts Stmt { $2 : $1 }

Stmt : AStmt ';' { $1 }

AStmt : SCall { $1 }
      | SDecl { $1 }

SDecl : varId                  { SDecl $1 Nothing Nothing }
      | varId ':' Type         { SDecl $1 Nothing Nothing }
      | varId ':' Type '=' Exp { SDecl $1 (Just $3) (Just $5) }

SCall : varId '(' ExpList0  ')' { SCall $1 $3 }

SReturn : return Exp { SReturn Exp }

ExpList0 : {- empty -} { [] }
         | ExpList     { $1 }

ExpList : Exp             { [$1] }
        | ExpList ',' Exp { $3 : $1 }

Exp : varId { EVar $1 }
    | Value { EVal $1 }

Type : varId { TVar $1 }
     | conId { TCon $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
