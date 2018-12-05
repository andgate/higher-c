{

{-# LANGUAGE OverloadedStrings #-}
module Language.HigherC.Parse where

import Language.HigherC.Lex
import Language.HigherC.Lex.Token
import Language.HigherC.Syntax.Concrete
import Language.HigherC.Syntax.Builtin
import Language.HigherC.Syntax.Location

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Set (Set)
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

qname : qvar_name { $1 }
      | qcon_name { $1 }

qvar_name : mod_path '::' var_id { mkQName $1 $3 }
qcon_name : mod_path '::' con_id { mkQName $1 $3 }

var_name : var_id { mkName $1 }
con_name : con_id { mkName $1 }
mod_name : con_name { $1 }

var_id : varId { extractId $1 }
con_id : conId { extractId $1 }

{-
mod_id : mod_id_r { reverse $1 }
mod_id_r
  : conId { [extractId $1] }
  | mod_id '.' con_id { (extractId $3) : $1 }
-}

value : integer  { fmap VInt    (extractInteger $1) }
      | double   { fmap VFloat  (extractDouble  $1) }
      | char     { fmap VChar   (extractChar    $1) }
      | string   { fmap VString (extractString  $1) }
      | boolean  { fmap VBool   (extractBool    $1) }


var_name_list: var_name_list_r { reverse $1 }
var_name_list_r
  : var_name                 { [$1] }
  | var_name_list_r var_name { $2 : $1 }

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
  | constructor_defn { TCtor $1 }
  | destructor_defn  { TDtor $1 }


-- -----------------------------------------------------------------------------
-- | Module Definition

module_defn : Module mod_path module_block { Module ($1 <> locOf $3) $2 $3 }

module_block : '{' top_level_stmts0 '}' { MBlock ($1 <> $3) $2 }

mod_path   : mod_path_r              { MPath (locOf $1) (NE.fromList $ reverse $1) }
mod_path_r : mod_name                { [$1] }
           | mod_path_r '.' mod_name { $3 : $1 }


-- -----------------------------------------------------------------------------
-- | Import

import_stmt : Import mod_path ';' { Import ($1 <> $3) $2 }


-- -----------------------------------------------------------------------------
-- | Declaration

-- Let-bound variable declarations
declaration
  : decl_head maybe_type_sig ';'                   { Decl1 (locOf $1 <> $3) $1 $2 }
  | decl_head maybe_type_sig '=' exp ';'           { Decl2 (locOf $1 <> $5) $1 $2 $4 }
  | decl_head '(' exp_args0 ')' maybe_type_sig ';' { Decl3 (locOf $1 <> $6) $1 $3 $5 }

decl_head
  : Let name  { DeclHead ($1 <> locOf $2) $2 }

maybe_type_sig
  : {- empty -}  { Nothing }
  | type_sig     { Just $1 }

type_sig
  : ':' type    { TypeSig ($1 <> locOf $2) $2 }


-- -----------------------------------------------------------------------------
-- | Function Definition

function_defn : function_decl block { FuncDefn (locOf $1 <> locOf $2) $1 $2 }

function_decl : maybe_func_specs var_name maybe_scheme '(' parameters0 ')' maybe_type_sig
              { FuncDecl (maybe (locOf $2) locOf $1 <> maybe $6 locOf $7) $1 $2 $3 $5 $7 }


-- | Function Specializers
maybe_func_specs
  : {- empty -}  { Nothing }
  | func_specs   { Just $1 }

func_specs : func_specs_list { FuncSpecs (locOf $1) (NE.fromList $1) }

func_specs_list : func_specs_rlist  { reverse $1 }
func_specs_rlist
  : func_spec              { [$1] }
  | func_specs_rlist func_spec { $2 : $1 }

func_spec
  : Inline    { InlineFunc $1    }
  | Recursive { RecursiveFunc $1 }

-- | Function Parameters
parameters0
  : {- empty -} { Parameters [] }
  | parameters  { Parameters $1 }

parameters : parameters_r { reverse $1 }

parameters_r
  : parameter                  { [$1] }
  | parameters_r ',' parameter { $3 : $1 }

parameter
  : var_name maybe_type_sig   { let l1 = locOf $1
                                in Parameter (l1 <> maybe l1 locOf $2) $1 $2
                              }


-- -----------------------------------------------------------------------------
-- | Constructor and Destructor Definitions

constructor_defn :
   constructor_decl block { CtorDefn (locOf $1 <> locOf $2) $1 $2 } 

constructor_decl :
  con_name '(' parameters0 ')' maybe_init_list { CtorDecl (locOf $1 <> maybe $4 locOf $5) $1 $3 $5 }

destructor_defn :
  destructor_decl block { DtorDefn (locOf $1 <> locOf $2) $1 $2 }

destructor_decl :
  '~' con_name  '(' parameters0 ')' { DtorDecl ($1 <> $5) $2 $4 }

maybe_init_list
  : {- empty -}    { Nothing }
  | ':' inits      { Just (InitList ($1 <> locOf $2) $2) }

init_list : inits { InitList (locOf $1) $1 }

inits : inits_r { reverse $1 }
inits_r
  : initializer             { [$1] }
  | inits_r ',' initializer { $3 : $1 }

initializer
  : name '(' exp_args0 ')' { Init (locOf $1 <> $4) $1 $3 }

-- -----------------------------------------------------------------------------
-- | Statements

stmt : stmt_exp      { $1 }
     | stmt_decl     { $1 }
     | stmt_block    { $1 }
     | stmt_with     { $1 }
     | stmt_break    { $1 }
     | stmt_continue { $1 }
     | stmt_return   { $1 }
     | stmt_if       { $1 }
     | stmt_while    { $1 }
     | stmt_do_while { $1 }
     | stmt_for      { $1 }
     | stmt_case     { $1 }


block : '{' '}'       { Block ($1 <> $2) [] }
      | '{' stmts '}' { Block ($1 <> $3) $2 }


stmts : stmts_r  { reverse $1 }
stmts_r
  : stmt         { [$1] }
  | stmts_r stmt { $2 : $1 }


stmt_exp
  : exp ';' { SExp (locOf $1 <> $2) $1 }

stmt_decl
  : maybe_stmt_decl_specs declaration { SDecl (maybe (locOf $2) locOf $1 <> locOf $2) $1 $2 }


maybe_stmt_decl_specs
  : {- empty -}      { Nothing }
  | stmt_decl_specs { Just $1 }

stmt_decl_specs : stmt_decl_specs_list { SDeclSpecs (locOf $1) (NE.fromList $1) }

stmt_decl_specs_list : stmt_decl_specs_rlist { reverse $1 }
stmt_decl_specs_rlist
  : stmt_decl_spec                       { [$1] }
  | stmt_decl_specs_rlist stmt_decl_spec { $2 : $1 }

stmt_decl_spec
  : Static { StaticDecl $1 }


stmt_block : block { SBlock (locOf $1) $1 }

stmt_with
  : With '(' exp ')' stmt { SWith ($1 <> locOf $5) $3 $5 } 

stmt_break    :      Break ';' { SBreak    ($1 <> $2) }
stmt_continue :   Continue ';' { SContinue ($1 <> $2) }
stmt_return   : Return exp ';' { SReturn   ($1 <> $3) $2 }


stmt_if
  : If '(' exp ')' stmt %prec IFX { SIf ($1 <> locOf $5) $3 $5 Nothing }
  | If '(' exp ')' stmt stmt_elif { SIf ($1 <> locOf $6) $3 $5 (Just $6) }
  | If '(' exp ')' stmt stmt_else { SIf ($1 <> locOf $6) $3 $5 (Just $6) }

stmt_elif
  : Elif '(' exp ')' stmt %prec IFX { SIf $3 $5 Nothing }
  | Elif '(' exp ')' stmt stmt_elif { SIf $3 $5 (Just $6) }
  | Elif '(' exp ')' stmt stmt_else { SIf $3 $5 (Just $6) }

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


stmt_case
  : Case '(' exp ')' case_alts { SCase ($1 <> locOf $5) $3 $5 }

case_alts : '{' case_alt_list '}' { Alts ($1 <> $3) (NE.fromList $2) }

case_alt_list : case_alt_rlist { reverse $1 }
case_alt_rlist
  : case_alt                { [$1] } 
  | case_alt_rlist case_alt { $2 : $1 }

case_alt
  : pat stmt { Alt (locOf $1 <> locOf $2) $1 $2 }


maybe_exp
  : {- empty -} { Nothing }
  | exp         { Just $1 }


-- -----------------------------------------------------------------------------
-- | Patterns

pat : pvar { $1 }

pvar : var_name { PVar $1 }

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

exp_var : var_name { EVar $1 }

exp_con : con_name { ECon $1 }

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

-- -----------------------------------------------------------------------------
-- | Type Predicates

pred
  : pred_forall { $1 }
  | pred_isIn   { $1 }

pred_forall
  : var_name { Forall (locOf $1) $1 }

pred_isIn
  : con_name '(' type_list ')' { IsIn (locOf $1 <> $4) $1 (NE.fromList $3) }


pred_list0
  : {- empty -}   { [] }
  | pred_list     { $1 }

pred_list : pred_list_r  { reverse $1 }
pred_list_r
  : pred                 { [$1] }
  | pred_list_r ',' pred { $3 : $1 }

-- -----------------------------------------------------------------------------
-- | Type Scheme

scheme
  : '<' pred_list0 '>' { Scheme ($1 <> $3) $2 }

maybe_scheme
  : {- empty -}  { Nothing }
  | scheme       { Just $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
