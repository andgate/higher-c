{

{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Parse where

import Language.Hawk.Lex
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Concrete
import Language.Hawk.Syntax.Builtin
import Language.Hawk.Syntax.Location

import Data.Set
import qualified Data.Set as Set

}

%name parseTopLevel top_level
%tokentype { Token }
%error { parseError }


%token
  backslash          { Token (TokenRsvp "\\") _ $$ }
  '->'               { Token (TokenRsvp "->") _ $$ }
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

  TYPE               { Token (TokenRsvp "Type") _ $$ }
  Void               { Token (TokenRsvp "Void") _ $$ }
  I32                { Token (TokenRsvp "I32" ) _ $$ }

  Let                { Token (TokenRsvp "let"   ) _ $$ }
  Static             { Token (TokenRsvp "static") _ $$ }
  Inline             { Token (TokenRsvp "inline") _ $$ }
  Recursive          { Token (TokenRsvp "recursive") _ $$ }
  As                 { Token (TokenRsvp "as"    ) _ $$ }

  New                { Token (TokenRsvp "new"  ) _ $$ }
  Newer              { Token (TokenRsvp "newer") _ $$ }
  Delete             { Token (TokenRsvp "delete") _ $$ }

  Module             { Token (TokenRsvp "module") _ $$ }
  Import             { Token (TokenRsvp "import") _ $$ }

  Type               { Token (TokenRsvp "type" ) _ $$ }
  Class              { Token (TokenRsvp "class") _ $$ }
  Impl               { Token (TokenRsvp "impl" ) _ $$ }

  If                 { Token (TokenRsvp "if"  ) _ $$ }
  Else               { Token (TokenRsvp "else") _ $$ }
  Elif               { Token (TokenRsvp "elif") _ $$ }
  Case               { Token (TokenRsvp "case") _ $$ }

  Try                { Token (TokenRsvp "try"    ) _ $$ }
  Catch              { Token (TokenRsvp "catch"  ) _ $$ }
  Finally            { Token (TokenRsvp "finally") _ $$ }

  Return             { Token (TokenRsvp "return") _ $$ }
  Break              { Token (TokenRsvp "break" ) _ $$ }
  Continue           { Token (TokenRsvp "continue") _ $$ }

  With               { Token (TokenRsvp  "with") _ $$ }
  Do                 { Token (TokenRsvp    "do") _ $$ }
  While              { Token (TokenRsvp "while") _ $$ }
  For                { Token (TokenRsvp   "for") _ $$ }

  varId              { Token (TokenVarId  _) _ _ }
  conId              { Token (TokenConId  _) _ _ }

  primAdd            { Token (TokenPrimId "#add") _ $$ }
  primSub            { Token (TokenPrimId "#sub") _ $$ }

  integer            { Token (TokenInteger _) _ _ }
  double             { Token (TokenDouble  _) _ _ }
  char               { Token (TokenChar    _) _ _ }
  string             { Token (TokenString  _) _ _ }
  boolean            { Token (TokenBool    _) _ _ }


%nonassoc IFX
%nonassoc Elif
%nonassoc Else

%%

-- -----------------------------------------------------------------------------
-- | Names and Values

name : var_name { $1 }
     | con_name { $1 }

var_name : var_id { mkName $1 }
con_name : con_id { mkName $1 }

var_id : varId { extractId $1 }
con_id : conId { extractId $1 }


value : integer  { fmap VInt    (extractInteger $1) }
      | double   { fmap VFloat  (extractDouble  $1) }
      | char     { fmap VChar   (extractChar    $1) }
      | string   { fmap VString (extractString  $1) }
      | boolean  { fmap VBool   (extractBool    $1) }


-- -----------------------------------------------------------------------------
-- | Top Level

top_level : top_level_stmts0 { TopLevel $1 }

top_level_stmts0
  : {- empty -}     { [] }
  | top_level_stmts { $1 }

top_level_stmts : top_level_stmts_r { reverse $1 }

top_level_stmts_r
  : top_level_stmt { [$1] }
  | top_level_stmts_r top_level_stmt { $2 : $1 }

top_level_stmt
  : module_defn      { TModule $1 }
  | import_stmt      { TImport $1 }
  | declaration      { TDecl $1 }
  | function_defn    { TFuncDefn $1 }
  | constructor_defn { TCtorDefn $1 }
  | destructor_defn  { TDtorDefn $1 }


-- -----------------------------------------------------------------------------
-- | Module Definition

module_defn : Module mod_path '{' top_level_stmts0 '}' { Module ($1 <> $5) $2 $4 }

mod_path : mod_path_r { MPath (reverse $1) }

mod_path_r : mod_name              { [$1] }
           | mod_path_r '.' mod_name { $3 : $1 }

mod_name : con_name { $1 }


-- -----------------------------------------------------------------------------
-- | Import

import_stmt : Import mod_path ';' { Import $2 }


-- -----------------------------------------------------------------------------
-- | Declaration

-- Let-bound variable declarations
declaration
  : decl_head maybe_type_annot ';'                   { Decl2 (Init $1 []) $2 }
  | decl_head maybe_type_annot '=' exp ';'           { Decl1 $1 $2 $4 }
  | decl_head '(' exp_args0 ')' maybe_type_annot ';' { Decl2 (Init $1 $3) $5 }

-- Two kinds of declarations
decl_head
  : Let name  { $2 }

maybe_type_annot
  : {- empty -} { Nothing }
  | ':' type    { Just $2 }





-- -----------------------------------------------------------------------------
-- | Function Definition

function_defn : func_specs0 function_decl block { FuncDefn $1 $2 $3 }

function_decl : var_name '(' parameters0 ')' maybe_type_annot { FuncDecl $1 mempty $3 $5  }


-- | Function Specializers
func_specs0
  : {- empty -}  { Set.empty }
  | func_specs   { $1 } 

func_specs
  : func_spec            { Set.singleton $1 }
  | func_specs func_spec { Set.insert $2 $1 }

func_spec
  : Inline    { InlineFunc    }
  | Recursive { RecursiveFunc }

-- | Function Parameters
parameters0
  : {- empty -} { Parameters [] }
  | parameters  { Parameters $1 }

parameters : parameters_r { reverse $1 }

parameters_r
  : parameter                  { [$1] }
  | parameters_r ',' parameter { $3 : $1 }

parameter
  : var_name maybe_type_annot { Parameter $1 $2 }


-- -----------------------------------------------------------------------------
-- | Constructor and Destructor Definitions

constructor_defn :
  con_name '(' parameters0 ')' maybe_init_list block { CtorDefn $1 mempty $3 $4 $5 }

destructor_defn :
  '~' con_name '(' parameters0 ')' block { DtorDefn $2 $4 $6 }

maybe_init_list
  : {- empty -}   { [] }
  | ':' init_list { $2 }

init_list : init_list_r { reverse $1 }
init_list_r
  : initializer                 { [$1] }
  | init_list_r ',' initializer { $3 : $1 }

initializer
  : name '(' exp_args0 ')' { Init $1 $3 }

-- -----------------------------------------------------------------------------
-- | Statements

stmt : stmt_decl     { $1 }
     | stmt_block    { $1 }
     | stmt_with     { $1 }
     | stmt_break    { $1 }
     | stmt_continue { $1 }
     | stmt_return   { $1 }
     | stmt_if       { $1 }
     | stmt_while    { $1 }
     | stmt_do_while { $1 }
     | stmt_for      { $1 }





block : '{' '}'       { Block [] }
      | '{' stmts '}' { Block $2 }


stmts : stmts_r  { reverse $1 }
stmts_r
  : stmt         { [$1] } 
  | stmts_r stmt { $2 : $1 }


stmt_decl : stmt_decl_specs0 declaration { SDecl $1 $2 }

stmt_decl_specs0
  : {- empty -}     { Set.empty }
  | stmt_decl_specs { $1 } 

stmt_decl_specs
  : stmt_decl_spec                 { Set.singleton $1 }
  | stmt_decl_specs stmt_decl_spec { Set.insert $2 $1 }

stmt_decl_spec
  : Static { StaticDecl }


stmt_block : block { SBlock $1 }

stmt_with
  : With '(' exp ')' stmt { SWith ($1 <> locOf $5) $3 $5 } 

stmt_break    :      Break ';' { SBreak }
stmt_continue :   Continue ';' { SContinue }
stmt_return   : Return exp ';' { SReturn $2 }


stmt_if
  : If '(' exp ')' stmt %prec IFX { EIf $3 $5 Nothing }
  | If '(' exp ')' stmt stmt_elif { EIf $3 $5 (Just $6) }
  | If '(' exp ')' stmt stmt_else { EIf $3 $5 (Just $6) }

maybe_else
  : {- empty -} { Nothing }
  | stmt_elif   { $1 }
  | stmt_else   { $1 }


stmt_elif
  : Elif '(' exp ')' stmt %prec IFX { EIf $3 $5 Nothing }
  | Elif '(' exp ')' stmt stmt_elif { EIf $3 $5 (Just $6) }
  | Elif '(' exp ')' stmt stmt_else { EIf $3 $5 (Just $6) }

stmt_else
  : Else stmt { $2 }


stmt_while
  : While '(' exp ')' stmt        { SWhile $3 $5 } 

stmt_do_while
  : Do stmt While '(' exp ')' ';' { SDoWhile $2 }


stmt_for
  : For '(' for_init maybe_exp ';' maybe_exp ')' stmt { SFor $3 $4 $6 $7 }

for_init
  : maybe_exp ';'         { Left $1 }
  | declaration           { Right $1 }

maybe_exp
  : {- empty -} { Nothing }
  | exp         { Just $1 }


-- -----------------------------------------------------------------------------
-- | Expressions

exp : cexp { $1 }

cexp
  : bexp ':' type { EType $1 $3 }
  | bexp { $1 }

bexp
  : bexp '(' exp_args0 ')' { ECall $1 $3 }
  | bexp '.' name          { EMember $1 $3 }
  | aexp { $1 }

aexp : exp_var    { $1 }
     | exp_con    { $1 }
     | exp_value  { $1 }
     | exp_parens { $1 }

exp_var : var_id { mkVar $1 }

exp_con : con_id { mkCon $1 }

exp_value : value { mkVal $1 }

exp_parens : '(' exp ')' { EParens $2 }


exp_args0
  : {- empty -}  { [] }
  | exp_args     { $1 }

exp_args
  : exp_args_r         { reverse $1 }
exp_args_r
  : exp                { [$1]    }
  | exp_args_r ',' exp { $3 : $1 }


-- -----------------------------------------------------------------------------
-- | Types

type : dtype { $1 }

dtype
  : ctype            { $1 }
  | ctype kind_annot { TKind $1 $2 }

ctype
  : btype { $1 }
  | btype '->' ctype { TArr $1 $3 }

btype
  : atype { $1 }
  | btype '(' type_list ')' { TApp $1 $3 }


type_list : type_list_r  { reverse $1 }
type_list_r
  : type                 { [$1] }
  | type_list_r ',' type { $3 : $1 }

atype : var_id        { mkTVar $1 }
      | con_id        { mkTCon $1 }
      | '(' type ')'  { TParen $2 }


-- -----------------------------------------------------------------------------
-- | Kinds

kind
  : akind           { $1 }
  | kind '->' akind { KArr $1 $3 }


akind : TYPE { KType }

kind_annot
  : ':' kind { $2 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
