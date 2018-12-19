{

{-# LANGUAGE OverloadedStrings #-}
module Language.HigherC.Parse where

import Data.Text (Text, unpack)

import Language.HigherC.Lex
import Language.HigherC.Lex.Token
import Language.HigherC.Syntax.Concrete
import qualified Language.HigherC.Syntax.Extra.Primitive as Prim
import Language.HigherC.Syntax.Extra.Location

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

}

%name parseObject object
%tokentype { Token }
%error { parseError }


%token
  '::'               { Token (TokenRsvp "::") _ $$ }
  ':'                { Token (TokenRsvp ":") _ $$ }
  ';'                { Token (TokenRsvp ";") _ $$ }
  ','                { Token (TokenRsvp ",") _ $$ }
  '.'                { Token (TokenRsvp ".") _ $$ }
  '_'                { Token (TokenRsvp "_") _ $$ }
  '@'                { Token (TokenRsvp "@") _ $$ }


  '('                { Token (TokenRsvp "(") _ $$ }
  ')'                { Token (TokenRsvp ")") _ $$ }
  '[]'               { Token (TokenRsvp "[]") _ $$ }
  '['                { Token (TokenRsvp "[") _ $$ }
  ']'                { Token (TokenRsvp "]") _ $$ }
  '{'                { Token (TokenRsvp "{") _ $$ }
  '}'                { Token (TokenRsvp "}") _ $$ }



  '<.>'              { Token (TokenRsvp "<.>") _ $$ }
  '<.'               { Token (TokenRsvp "<.")  _ $$ }

  '->'               { Token (TokenOpId "->") _ $$ }
  '='                { Token (TokenOpId "=") _ $$ }
  '~'                { Token (TokenOpId "~") _ $$ }
  '<'                { Token (TokenOpId "<") _ _ }
  '>'                { Token (TokenOpId ">") _ _ }
  opId               { Token (TokenOpId   _) _ _ }

  TYPE               { Token (TokenConId "Type") _ $$ }
  VOID               { Token (TokenConId "Void") _ $$ }

  I1                 { Token (TokenConId "I1" ) _ $$ }
  I8                 { Token (TokenConId "I8" ) _ $$ }
  I16                { Token (TokenConId "I16" ) _ $$ }
  I32                { Token (TokenConId "I32" ) _ $$ }
  I64                { Token (TokenConId "I64" ) _ $$ }
  I128               { Token (TokenConId "I128" ) _ $$ }

  Fp16                { Token (TokenConId "Fp16" ) _ $$ }
  Fp32                { Token (TokenConId "Fp32" ) _ $$ }
  Fp64                { Token (TokenConId "Fp64" ) _ $$ }
  Fp128               { Token (TokenConId "Fp128" ) _ $$ }

  Operator           { Token (TokenRsvp "operator") _ $$ }
  Prefix             { Token (TokenRsvp "prefix") _ $$ }
  Infix              { Token (TokenRsvp "infix") _ $$ }
  Infixl             { Token (TokenRsvp "infixl") _ $$ }
  Infixr             { Token (TokenRsvp "infixr") _ $$ }
  Postfix            { Token (TokenRsvp "postfix") _ $$ }

  Static             { Token (TokenRsvp "static") _ $$ }
  Inline             { Token (TokenRsvp "inline") _ $$ }
  Recursive          { Token (TokenRsvp "rec") _ $$ }
  Extern             { Token (TokenRsvp "extern") _ $$ }

  New                { Token (TokenRsvp "new"  ) _ $$ }
  Renew              { Token (TokenRsvp "renew"  ) _ $$ }
  Delete             { Token (TokenRsvp "delete") _ $$ }

  Module             { Token (TokenRsvp "module") _ $$ }
  Import             { Token (TokenRsvp "import") _ $$ }

  Type               { Token (TokenRsvp "type" ) _ $$ }
  Alias              { Token (TokenRsvp "alias") _ $$ }
  Class              { Token (TokenRsvp "class") _ $$ }
  Inst               { Token (TokenRsvp "inst" ) _ $$ }

  Let                { Token (TokenRsvp "let"   ) _ $$ }
  As                 { Token (TokenRsvp "as"    ) _ $$ }
  Const              { Token (TokenRsvp "const" ) _ $$ }

  If                 { Token (TokenRsvp "if"  ) _ $$ }
  Else               { Token (TokenRsvp "else") _ $$ }
  Elif               { Token (TokenRsvp "elif") _ $$ }
  Case               { Token (TokenRsvp "case") _ $$ }

  Try                { Token (TokenRsvp "try"    ) _ $$ }
  Catch              { Token (TokenRsvp "catch"  ) _ $$ }
  Finally            { Token (TokenRsvp "finally") _ $$ }
  Throw              { Token (TokenRsvp "throw"  ) _ $$ }

  Return             { Token (TokenRsvp "return") _ $$ }
  Break              { Token (TokenRsvp "break" ) _ $$ }
  Continue           { Token (TokenRsvp "continue") _ $$ }

  With               { Token (TokenRsvp  "with") _ $$ }
  Do                 { Token (TokenRsvp    "do") _ $$ }
  While              { Token (TokenRsvp "while") _ $$ }
  For                { Token (TokenRsvp   "for") _ $$ }

  Null               { Token (TokenRsvp "null") _ $$ }

  varId              { Token (TokenVarId  _) _ _ }
  conId              { Token (TokenConId  _) _ _ }
  primId             { Token (TokenPrimId _) _ _ }

  integer            { Token (TokenInteger _) _ _ }
  double             { Token (TokenDouble  _) _ _ }
  char               { Token (TokenChar    _) _ _ }
  string             { Token (TokenString  _) _ _ }
  boolean            { Token (TokenBool    _) _ _ }

  eof                { Token TokenEof _ _ }

%nonassoc IFX
%nonassoc Elif
%nonassoc Else

%right opId

%%

-- -----------------------------------------------------------------------------
-- | Names

name :: { Name }
name : var_name { $1 }
     | con_name { $1 }
     | '(' op_name ')' { $2 }


qname :: { Name }
qname : qvar_name { $1 }
      | qcon_name { $1 }

qvar_name :: { Name }
qvar_name : mod_path '::' var_id { mkQName $1 $3 }

qcon_name :: { Name }
qcon_name : mod_path '::' con_id { mkQName $1 $3 }

var_name  :: { Name }
var_name : var_id { mkName $1 }

con_name :: { Name }
con_name : con_id { mkName $1 }

mod_name :: { Name }
mod_name : con_name { $1 }

op_name :: { Name }
op_name  : op_id  { mkName $1 }

op_name_ext :: { Name }
op_name_ext  : op_id_ext  { mkName $1 }


-- Text Identifiers
var_id :: { L Text }
var_id : varId { extractId $1 }

con_id :: { L Text }
con_id : conId { extractId $1 }

mod_id :: { L Text }
mod_id : con_id { $1 }

op_id :: { L Text }
op_id : opId       { extractId   $1 }

op_id_ext :: { L Text }
op_id_ext 
  : op_id           { $1 }
  | '->'            { L $1 "->" }
  | '='             { L $1 "=" }
  | '~'             { L $1 "~" }
  | '<'             { extractRsvp $1 }
  | '>'             { extractRsvp $1 }

prim_id :: { L Text }
prim_id : primId { extractId $1 }


-- -----------------------------------------------------------------------------
-- | Values

value :: { L (Prim.Value Exp) }
value : Null     { L $1 Prim.VNull }
      | boolean  { fmap Prim.VBool   (extractBool    $1) } 
      | integer  { fmap Prim.VInt    (extractInteger $1) }
      | double   { fmap Prim.VFp     (extractDouble  $1) }
      | char     { fmap Prim.VChar   (extractChar    $1) }
      | '[]'     { L $1 (Prim.VVector []) }
      | '<.>'    { L $1 (Prim.VVector []) }
      | '['  exp_args0 ']' { L ($1<>$3) (Prim.VArray $2) }
      | '<.' exp_args0 '>' { L ($1<> locOf $3) (Prim.VVector $2) }
      | string             { fmap Prim.VString (extractString  $1) }


var_name_list :: { [Name] } 
var_name_list : var_name_list_r { reverse $1 }

var_name_list_r :: { [Name] }
var_name_list_r
  : var_name                 { [$1] }
  | var_name_list_r var_name { $2 : $1 }


-- -----------------------------------------------------------------------------
-- | Syntax Object

object :: { Object }
object
  : object_base        { $1 }
  | object object_base { $1 <> $2 }

object_base :: { Object }
object_base
  : module_stmts  { Object Nothing $1 []   []   []   }
  | module_defn   { Object Nothing [] [$1] []   []   }
  | import_stmt   { Object Nothing [] []   [$1] []   }
  | operator_decl { Object Nothing [] []   []   [$1] }

module_defn :: { Module }
module_defn
  : Module mod_path '{' module_body '}' {  ($4){ modName = $2, modLoc = $1 <> $5 } }

module_body :: { Module }
module_body
  : module_base             { $1 }
  | module_body module_base { $1 <> $2 }

module_base :: { Module }
module_base
  : module_stmts  { Module rootModPath mempty $1 []   []   []   }
  | module_defn   { Module rootModPath mempty [] [$1] []   []   }
  | import_stmt   { Module rootModPath mempty [] []   [$1] []   }
  | operator_decl { Module rootModPath mempty [] []   []   [$1] }

module_stmts0 :: { [ModuleStmt] }
module_stmts0
  : {- empty -}     { [] }
  | module_stmts    { $1 }

module_stmts :: { [ModuleStmt] }
module_stmts : module_stmts_r { reverse $1 }

module_stmts_r :: { [ModuleStmt] }
module_stmts_r
  : module_stmt { [$1] }
  | module_stmts_r module_stmt { $2 : $1 }

module_stmt :: { ModuleStmt }
module_stmt
  : declaration      { MDecl $1 }
  | function_defn    { MFuncDefn $1 }
  | function_extern  { MFuncExtern $1 }
  | constructor_defn { MCtor $1 }
  | destructor_defn  { MDtor $1 }
  | type_defn        { MTypeDefn $1 }
  | alias_defn       { MAliasDefn $1 }
  | class_defn       { MClass $1 }
  | instance_defn    { MInst $1 }



-- -----------------------------------------------------------------------------
-- | Module Definition

mod_path :: { ModulePath }
mod_path
  : mod_path_text { MPath (locOf $1) (NE.fromList $ unL $1) }

mod_path_text :: { L [Text] }
mod_path_text
  : mod_id                   { fmap pure $1       }
  | mod_path_text '.' mod_id { $1 <> (fmap pure $3) }  -- Challenge: Use massive module names, break compiler.


-- -----------------------------------------------------------------------------
-- | Import

import_stmt :: { Import }
import_stmt : Import mod_path ';' { Import ($1 <> $3) $2 }


-- -----------------------------------------------------------------------------
-- | Declaration

-- Let-bound variable declarations
declaration :: { Decl }
declaration
  : decl_head maybe_type_sig ';'                   { Decl1 (locOf $1 <> $3) $1 $2 }
  | decl_head maybe_type_sig '=' exp ';'           { Decl2 (locOf $1 <> $5) $1 $2 $4 }
  | decl_head '(' exp_args0 ')' maybe_type_sig ';' { Decl3 (locOf $1 <> $6) $1 $3 $5 }

decl_head :: { DeclHead }
decl_head
  : Let name  { DeclHead ($1 <> locOf $2) $2 }

maybe_type_sig :: { Maybe TypeSig }
maybe_type_sig
  : {- empty -}  { Nothing }
  | type_sig     { Just $1 }

type_sig :: { TypeSig }
type_sig
  : ':' type    { TypeSig ($1 <> locOf $2) $2 }


-- -----------------------------------------------------------------------------
-- | Function Definition

function_defn :: { FuncDefn }
function_defn : function_decl block { FuncDefn (locOf $1 <> locOf $2) $1 $2 }

function_decl :: { FuncDecl }
function_decl : maybe_func_specs var_name maybe_scheme '(' parameters0 ')' maybe_type_sig
              { FuncDecl (maybe (locOf $2) locOf $1 <> maybe $6 locOf $7) $1 $2 $3 $5 $7 }


function_extern :: { FuncExtern }
function_extern
  : Extern var_name '(' parameters0 ')' type_sig ';'
    { FuncExtern ($1 <> $7) $2 $4 $6 }

-- | Function Specializers
maybe_func_specs :: { Maybe FuncSpecs }
maybe_func_specs
  : {- empty -}  { Nothing }
  | func_specs   { Just $1 }

func_specs :: { FuncSpecs }
func_specs : func_specs_list { FuncSpecs (locOf $1) (NE.fromList $1) }

func_specs_list :: { [FuncSpec] }
func_specs_list : func_specs_list_r  { reverse $1 }

func_specs_list_r :: { [FuncSpec] }
func_specs_list_r
  : func_spec                   { [$1] }
  | func_specs_list_r func_spec { $2 : $1 }

func_spec :: { FuncSpec }
func_spec
  : Inline    { InlineFunc $1    }
  | Recursive { RecursiveFunc $1 }

-- | Function Parameters
parameters0 :: { Parameters }
parameters0
  : {- empty -} { Parameters [] }
  | parameters  { $1 }

parameters :: { Parameters }
parameters : parameters_list_r { Parameters (reverse $1) }

parameters_list_r :: { [Parameter] }
parameters_list_r
  : parameter                  { [$1] }
  | parameters_list_r ',' parameter { $3 : $1 }

parameter :: { Parameter }
parameter
  : var_name maybe_type_sig   { let l1 = locOf $1
                                in Parameter (l1 <> maybe l1 locOf $2) $1 $2
                              }


-- -----------------------------------------------------------------------------
-- | Constructor and Destructor Definitions

constructor_defn :: { CtorDefn }
constructor_defn :
   constructor_decl block { CtorDefn (locOf $1 <> locOf $2) $1 $2 } 

constructor_decl :: { CtorDecl }
constructor_decl :
  con_name '(' parameters0 ')' maybe_init_list { CtorDecl (locOf $1 <> maybe $4 locOf $5) $1 $3 $5 }


destructor_defn :: { DtorDefn }
destructor_defn :
  destructor_decl block { DtorDefn (locOf $1 <> locOf $2) $1 $2 }

destructor_decl :: { DtorDecl }
destructor_decl :
  '~' con_name  '(' parameters0 ')' { DtorDecl ($1 <> $5) $2 $4 }


maybe_init_list :: { Maybe Inits }
maybe_init_list
  : {- empty -}    { Nothing }
  | inits      { Just $1 }

inits :: { Inits }
inits : ':' init_list { Inits ($1 <> locOf $2) (NE.fromList $2) }

init_list :: { [Initializer] }
init_list : init_list_r { reverse $1 }

init_list_r :: { [Initializer] }
init_list_r
  : initializer                 { [$1] }
  | init_list_r ',' initializer { $3 : $1 }


initializer :: { Initializer }
initializer
  : name '(' exp_args0 ')' { Init (locOf $1 <> $4) $1 $3 }

-- -----------------------------------------------------------------------------
-- | Statements

stmt :: { Stmt }
stmt : stmt_nop    { $1 }
     | stmt_exp      { $1 }
     | stmt_decl     { $1 }
     | stmt_block    { $1 }
     | stmt_with     { $1 }
     | stmt_break    { $1 }
     | stmt_continue { $1 }
     | stmt_return   { $1 }
     | stmt_throw    { $1 }
     | stmt_if       { $1 }
     | stmt_while    { $1 }
     | stmt_do_while { $1 }
     | stmt_for      { $1 }
     | stmt_case     { $1 }
     | stmt_try_catch { $1 }


block :: { Block }
block : '{' '}'       { Block ($1 <> $2) [] }
      | '{' stmts '}' { Block ($1 <> $3) $2 }


stmts :: { [Stmt] }
stmts : stmts_r  { reverse $1 }
stmts_r
  : stmt         { [$1] }
  | stmts_r stmt { $2 : $1 }

stmt_nop :: { Stmt }
stmt_nop
  : ';' { SNop $1 }

stmt_exp :: { Stmt }
stmt_exp
  : exp ';' { SExp (locOf $1 <> $2) $1 }

stmt_decl :: { Stmt }
stmt_decl
  : maybe_stmt_decl_specs declaration { SDecl (maybe (locOf $2) locOf $1 <> locOf $2) $1 $2 }


maybe_stmt_decl_specs :: { Maybe SDeclSpecs }
maybe_stmt_decl_specs
  : {- empty -}      { Nothing }
  | stmt_decl_specs { Just $1 }

stmt_decl_specs :: { SDeclSpecs }
stmt_decl_specs : stmt_decl_specs_list { SDeclSpecs (locOf $1) (NE.fromList $1) }

stmt_decl_specs_list :: { [SDeclSpec] }
stmt_decl_specs_list : stmt_decl_specs_list_r { reverse $1 }

stmt_decl_specs_list_r :: { [SDeclSpec] }
stmt_decl_specs_list_r
  : stmt_decl_spec                       { [$1] }
  | stmt_decl_specs_list_r stmt_decl_spec { $2 : $1 }

stmt_decl_spec :: { SDeclSpec }
stmt_decl_spec
  : Static { StaticDecl $1 }


stmt_block :: { Stmt }
stmt_block : block { SBlock (locOf $1) $1 }

stmt_with :: { Stmt }
stmt_with
  : With '(' exp ')' stmt { SWith ($1 <> locOf $5) $3 $5 } 

stmt_break :: { Stmt }
stmt_break    :      Break ';' { SBreak    ($1 <> $2) }

stmt_continue :: { Stmt }
stmt_continue :   Continue ';' { SContinue ($1 <> $2) }

stmt_return :: { Stmt }
stmt_return   : Return may_exp ';' { SReturn   ($1 <> $3) $2 }


stmt_throw :: { Stmt }
stmt_throw
  : Throw exp ';' { SThrow ($1 <> $3) $2}

stmt_if :: { Stmt }
stmt_if
  : If '(' exp ')' stmt %prec IFX { SIf ($1 <> locOf $5) $3 $5 Nothing }
  | If '(' exp ')' stmt stmt_elif { SIf ($1 <> locOf $6) $3 $5 (Just $6) }
  | If '(' exp ')' stmt stmt_else { SIf ($1 <> locOf $6) $3 $5 (Just $6) }

stmt_elif :: { Stmt }
stmt_elif
  : Elif '(' exp ')' stmt %prec IFX { SIf ($1 <> locOf $5) $3 $5 Nothing   }
  | Elif '(' exp ')' stmt stmt_elif { SIf ($1 <> locOf $6) $3 $5 (Just $6) }
  | Elif '(' exp ')' stmt stmt_else { SIf ($1 <> locOf $6) $3 $5 (Just $6) }

stmt_else :: { Stmt }
stmt_else
  : Else stmt { $2 }


stmt_while :: { Stmt }
stmt_while
  : While '(' exp ')' stmt        { SWhile ($1 <> locOf $5) $3 $5 } 

stmt_do_while :: { Stmt }
stmt_do_while
  : Do stmt While '(' exp ')' ';' { SDoWhile ($1 <> $7) $2 $5 }


stmt_for :: { Stmt }
stmt_for
  : For '(' for_init may_exp ';' may_exp ')' stmt { SFor ($1 <> locOf $8) $3 $4 $6 $8 }

for_init :: { Either (Maybe Exp) Decl }
for_init
  : may_exp ';'           { Left $1 }
  | declaration           { Right $1 }


stmt_case :: { Stmt }
stmt_case
  : Case '(' exp ')' case_alts { SCase ($1 <> locOf $5) $3 $5 }

case_alts :: { Alts }
case_alts : '{' case_alt_list '}' { Alts ($1 <> $3) (NE.fromList $2) }

case_alt_list :: { [Alt] }
case_alt_list : case_alt_list_r { reverse $1 }

case_alt_list_r :: { [Alt] }
case_alt_list_r
  : case_alt                { [$1] } 
  | case_alt_list_r case_alt { $2 : $1 }

case_alt :: { Alt }
case_alt
  : pat stmt { Alt (locOf $1 <> locOf $2) $1 $2 }



stmt_try_catch :: { Stmt }
stmt_try_catch
  : stmt_try stmt_catch_list may_stmt_finally
    { case $3 of
        Nothing
          | null $2   -> STryCatch (locOf $1) $1 $2 $3
          | otherwise -> STryCatch (locOf $1 <> locOf $2) $1 $2 $3
        Just _ -> STryCatch (locOf $1 <> locOf $3) $1 $2 $3
    }

stmt_try :: { Try }
stmt_try
  : Try stmt { Try ($1 <> locOf $2) $2 }

stmt_catch :: { Catch }
stmt_catch
  : Catch '(' exp ')' stmt { Catch ($1 <> locOf $5) $3 $5 }

stmt_catch_list0 :: { [Catch] }
stmt_catch_list0
  : {- empty -}     { [] }
  | stmt_catch_list { $1 }

stmt_catch_list :: { [Catch] }
stmt_catch_list
  : stmt_catch_list_r { reverse $1 }

stmt_catch_list_r :: { [Catch] }
stmt_catch_list_r
  : stmt_catch                   { [$1] }
  | stmt_catch_list_r stmt_catch { $2 : $1 }

stmt_finally :: { Finally }
stmt_finally
  : Finally stmt { Finally ($1 <> locOf $2) $2 }

may_stmt_finally :: { Maybe Finally }
may_stmt_finally
  : {- empty -}  { Nothing }
  | stmt_finally { Just $1 }



-- -----------------------------------------------------------------------------
-- | Patterns

pat :: { Pat }
pat : bpat { $1 }

cpat :: { Pat }
cpat
  : pat type_sig { PType (locOf $1 <> locOf $2) $1 $2 }
  | bpat { $1 }

bpat :: { Pat }
bpat
  : var_name '@' bpat                        { PAs  (locOf $1 <> locOf $3) $1 $3 }
  | con_name '(' pat_list0 ')'               { PCon (locOf $1 <> $4)       $1 $3 }
  | con_name '{' pat_rec_field_list0 '}'     { PRec (locOf $1 <> $4)       $1 $3 }
  | apat { $1 }

apat :: { Pat }
apat
  : pat_var     { $1 }
  | '(' pat ')' { PParen ($1 <> $3) $2 }
  | '_'         { PWild $1 }

pat_var :: { Pat }
pat_var : var_name { PVar $1 }


pat_rec_field :: { PatRecField }
pat_rec_field
  : var_name '=' pat { PatRecField (locOf $1 <> locOf $3) $1 $3 }

pat_rec_field_list0 :: { [PatRecField] }
pat_rec_field_list0
  : {- empty -}        { [] }
  | pat_rec_field_list { $1 }

pat_rec_field_list :: { [PatRecField] }
pat_rec_field_list : pat_rec_field_list_r { reverse $1 }

pat_rec_field_list_r :: { [PatRecField] }
pat_rec_field_list_r
  : pat_rec_field                          {    [$1] }
  | pat_rec_field_list_r ',' pat_rec_field { $3 : $1 }


pat_list0 :: { [Pat] }
pat_list0
  : {- empty -} { [] }
  | pat_list    { $1 }

pat_list :: { [Pat] }
pat_list : pat_list_r { reverse $1 }

pat_list_r :: { [Pat] }
pat_list_r
  : pat                {    [$1] }
  | pat_list_r ',' pat { $3 : $1 }


-- -----------------------------------------------------------------------------
-- | Expressions

exp :: { Exp }
exp : dexp { $1 }


dexp :: { Exp }
dexp
  : cexp type_sig { EType (locOf $1 <> locOf $2) $1 $2 }
  | cexp As type  { EAs (locOf $1 <> locOf $3) $1 $3 }
  | cexp { $1 }

cexp :: { Exp }
cexp
  : exp_op(bexp) { $1 }

exp_op(ex) : cexpop(ex) { EOp (locOf $1) $1 }

cexpop(ex)
  : bexpop(ex)                        { $1 }
  | cexpop(ex) op_name_ext bexpop(ex) { $1 ++ [Operator $2] ++ $3 }

bexpop(ex)
  : op_name_ext aexpop(ex)   { [Operator $1] ++ $2 }
  | aexpop(ex)               { $1 }

aexpop(ex)
  : ex op_name_ext     { [Operand $1, Operator $2]  }
  | ex                 { [Operand $1] }

bexp :: { Exp }
bexp
  : bexp arguments         { ECall         (locOf $1 <> locOf $2) $1 $2 }
  | bexp '.' var_name      { EMember       (locOf $1 <> locOf $3) $1 $3 }
  | bexp '->' var_name     { EPtrAccess    (locOf $1 <> locOf $3) $1 $3 }
  | bexp '[' exp ']'       { EArrayAccess  (locOf $1 <> $4) $1 $3       }
  | bexp '<.' exp '>'      { EVectorAccess (locOf $1 <> locOf $4) $1 $3 }
  | New aexp               { ENew ($1 <> locOf $2) $2 }
  | Renew aexp             { ERenew ($1 <> locOf $2) $2 }
  | Delete aexp            { EDelete ($1 <> locOf $2) $2 }
  | aexp                   { $1 }

aexp :: { Exp }
aexp : exp_var    { $1 }
     | exp_con    { $1 }
     | exp_value  { $1 }
     | exp_instr  { $1 }
     | exp_parens { $1 }


exp_var :: { Exp }
exp_var : var_name { EVar $1 }

exp_con :: { Exp }
exp_con : con_name { ECon $1 }

exp_value :: { Exp }
exp_value : value { mkVal $1 }

exp_parens :: { Exp }
exp_parens : '(' exp ')' { EParens ($1 <> $3) $2 }


may_exp :: { Maybe Exp }
may_exp
  : {- empty -} { Nothing }
  | exp         { Just $1 }


arguments :: { Arguments }
arguments
  : '(' exp_args0 ')' { Arguments ($1 <> $3) $2 }

exp_args0 :: { [Exp] }
exp_args0
  : {- empty -}  { [] }
  | exp_args     { $1 }

exp_args :: { [Exp] }
exp_args
  : exp_args_r         { reverse $1 }

exp_args_r :: { [Exp] }
exp_args_r
  : exp                { [$1]    }
  | exp_args_r ',' exp { $3 : $1 }


exp_instr :: { Exp }
exp_instr
  : prim_id '(' exp ',' exp ')' { EInstr (locOf $1 <> $6) (Prim.readPrimInstr $3 $5 (unL $1)) }



-- -----------------------------------------------------------------------------
-- | Types

type :: { Type }
type : etype { $1 }

-- Kind signature
etype :: { Type }
etype
  : ctype            { $1 }
  | ctype kind_sig   { TKind (locOf $1 <> locOf $2) $1 $2 }

-- Complex Constructors (that group operators)
ctype :: { Type }
ctype
  : ctype '->' type_op(btype) { TArr (locOf $1 <> locOf $3) $1 $3 }
  | type_op(btype)            { $1 }


type_op(ty) : ctyop(ty) { TOp (locOf $1) $1 }

ctyop(ty)
  : btyop(ty)                       { $1 }
  | ctyop(ty) op_name_ext btyop(ty) { $1 ++ [TyOperator $2] ++ $3 }

btyop(ty)
  : op_name_ext atyop(ty)   { [TyOperator $1] ++ $2 }
  | atyop(ty)               { $1 }

atyop(ty)
  : ty op_name_ext     { [TyOperand $1, TyOperator $2]  }
  | ty                 { [TyOperand $1] }


btype :: { Type }
btype
  : type_con_const(btype)    { TPrimCon (locOf $1) $1 }
  | type_con_array(btype)    { TPrimCon (locOf $1) $1 }
  | type_con_vector(btype)   { TPrimCon (locOf $1) $1 }
  | atype maybe_scheme type_arguments { TApp (locOf $1 <> locOf $3) $1 $2 $3 }
  | atype { $1 }

atype :: { Type }
atype : var_name        { TVar $1 }
      | con_name        { TCon $1 }
      | type_con_void   { TPrimCon (locOf $1) $1 }
      | type_con_int    { TPrimCon (locOf $1) $1 }
      | type_con_fp     { TPrimCon (locOf $1) $1 }
      | '(' type ')'    { TParens ($1 <> $3) $2 }


type_arguments :: { TypeArguments }
type_arguments
  : '(' type_list0 ')' { TypeArguments ($1 <> $3) $2 }

type_list0 :: { [Type] }
type_list0
  : {- empty -} { [] }
  | type_list   { $1 }

type_list :: { [Type] }
type_list : type_list_r  { reverse $1 }

type_list_r :: { [Type] }
type_list_r
  : type                 { [$1] }
  | type_list_r ',' type { $3 : $1 }


type_parameter :: { TypeParameter }
type_parameter
  : var_name maybe_kind_sig
    { case $2 of
        Nothing  -> TypeParameter (locOf $1) $1 $2
        Just sig -> TypeParameter (locOf $1 <> locOf sig) $1 $2
    }

type_parameters :: { TypeParameters }
type_parameters
  : '(' type_parameter_list0 ')' { TypeParameters ($1 <> $3) $2 }

type_parameter_list0 :: { [TypeParameter] }
type_parameter_list0
  : {- empty -}           { [] }
  | type_parameter_list   { $1 }

type_parameter_list :: { [TypeParameter] }
type_parameter_list : type_parameter_list_r  { reverse $1 }

type_parameter_list_r :: { [TypeParameter] }
type_parameter_list_r
  : type_parameter                           { [$1] }
  | type_parameter_list_r ',' type_parameter { $3 : $1 }

-- -----------------------------------------------------------------------------
-- | Primitive Type Constructors

type_con_void :: { Prim.TypeCon Type Exp }
type_con_void
  : VOID   { Prim.TVoid $1 }

type_con_int :: { Prim.TypeCon Type Exp }
type_con_int
  : I1     { Prim.TInt $1 1   }
  | I8     { Prim.TInt $1 8   }
  | I16    { Prim.TInt $1 16  }
  | I32    { Prim.TInt $1 32  }
  | I64    { Prim.TInt $1 64  }
  | I128   { Prim.TInt $1 128 }

type_con_fp :: { Prim.TypeCon Type Exp }
type_con_fp
  : Fp16    { Prim.TFp $1 16  }
  | Fp32    { Prim.TFp $1 32  }
  | Fp64    { Prim.TFp $1 64  }
  | Fp128   { Prim.TFp $1 128 }


type_con_array(ty)
  : ty '[]'        { Prim.TArray (locOf $1 <> $2) $1 }
  | ty '[' exp ']' { Prim.TArraySized (locOf $1 <> $4) $1 $3}

type_con_vector(ty)
  : ty '<.>'        { Prim.TVector (locOf $1 <> $2) $1 }
  | ty '<.' exp '>' { Prim.TVectorSized (locOf $1 <> locOf $4) $1 $3 }

type_con_const(ty)
  : Const ty { Prim.TConst ($1 <> locOf $2) $2 }

-- -----------------------------------------------------------------------------
-- | Kinds

kind :: { Kind }
kind
  : akind           { $1 }
  | kind '->' akind { KArr (locOf $1 <> locOf $3) $1 $3 }

akind :: { Kind }
akind : TYPE { KType $1 }

kind_sig :: { KindSig }
kind_sig
  : ':' kind { KindSig ($1 <> locOf $2) $2 }

maybe_kind_sig :: { Maybe KindSig }
maybe_kind_sig
  : {- empty -} { Nothing }
  | kind_sig    { Just $1 }

-- -----------------------------------------------------------------------------
-- | Type Scheme

scheme :: { Scheme }
scheme
  : '<' pred_list0 '>' { Scheme (locOf $1 <> locOf $3) $2 }

maybe_scheme :: { Maybe Scheme }
maybe_scheme
  : {- empty -}  { Nothing }
  | scheme       { Just $1 }


-- -----------------------------------------------------------------------------
-- | Type Predicates

pred :: { Pred }
pred
  : pred_forall { $1 }
  | pred_isIn   { $1 }

pred_forall :: { Pred }
pred_forall
  : var_name { Forall (locOf $1) $1 }

pred_isIn :: { Pred }
pred_isIn
  : con_name '(' type_list ')' { IsIn (locOf $1 <> $4) $1 (NE.fromList $3) }


pred_list0 :: { [Pred] }
pred_list0
  : {- empty -}   { [] }
  | pred_list     { $1 }

pred_list :: { [Pred] }
pred_list : pred_list_r  { reverse $1 }

pred_list_r :: { [Pred] }
pred_list_r
  : pred                 { [$1] }
  | pred_list_r ',' pred { $3 : $1 }


------------------------------------------------------------------------
-- Type Definition

type_defn :: { TypeDefn }
type_defn
  : type_decl type_defn_body { TypeDefn (locOf $1 <> locOf $2) $1 $2  }


type_decl :: { TypeDecl }
type_decl
  : Type con_name maybe_scheme type_parameters { TypeDecl ($1 <> locOf $4) $2 $3 $4 }


type_defn_body :: { TyDefnBody }
type_defn_body
  : '{' data_defn_list0 '}' { TyDefnBody ($1 <> $3) $2 }
  | ';'                    { TyDefnBody $1 mempty     }

data_defn :: { DataDefn }
data_defn
  : con_name data_fields ';' { DataDefn (locOf $1 <> $3) $1 $2 }
  | con_name object_fields   { ObjectDefn (locOf $1 <> locOf $2) $1 $2 }

data_defn_list0 :: { [DataDefn] }
data_defn_list0
  : {- empty -}      { [] }
  | data_defn_list   { $1 }

data_defn_list :: { [DataDefn] }
data_defn_list : data_defn_list_r { reverse $1 }

data_defn_list_r :: { [DataDefn] }
data_defn_list_r
  : data_defn                  {    [$1] }
  | data_defn_list_r data_defn { $2 : $1 }

--------------------------------------------------------------------
-- Data notation

data_fields :: { DataFields }
data_fields
  : '(' data_field_list0 ')'  { DataFields ($1 <> $3) $2 }

data_field_list0 :: { [DataField] }
data_field_list0
  : {- empty -}     { [] }
  | data_field_list { $1 }

data_field_list :: { [DataField] }
data_field_list : data_field_list_r { reverse $1 }

data_field_list_r :: { [DataField] }
data_field_list_r
  : data_field                       {    [$1] }
  | data_field_list_r ',' data_field { $3 : $1 }

data_field :: { DataField }
data_field
  : type maybe_data_field_default
    { case $2 of
        Nothing -> DataField (locOf $1) $1 $2
        Just d  -> DataField (locOf $1 <> locOf d) $1 $2
    }

data_field_default :: { DataFieldDefault }
data_field_default
  : '=' exp { DataFieldDefault ($1 <> locOf $2) $2 }

maybe_data_field_default :: { Maybe DataFieldDefault }
maybe_data_field_default
  : {- empty -}        { Nothing }
  | data_field_default { Just $1 }


--------------------------------------------------------------------
-- Object notation

object_fields :: { ObjectFields }
object_fields
  : '{' object_field_list0 '}'  { ObjectFields ($1 <> $3) $2 }

object_field_list0 :: { [ObjectField] }
object_field_list0
  : {- empty -}     { [] }
  | object_field_list { $1 }

object_field_list :: { [ObjectField] }
object_field_list
  : object_field_list_r { reverse $1 }

object_field_list_r :: { [ObjectField] }
object_field_list_r
  : object_field                         {    [$1] }
  | object_field_list_r ',' object_field { $3 : $1 }

object_field :: { ObjectField }
object_field
  : var_name type_sig maybe_data_field_default
    { case $3 of
        Nothing -> ObjectField (locOf $1 <> locOf $2) $1 $2 $3
        Just d  -> ObjectField (locOf $1 <> locOf d) $1 $2 $3
    }


--------------------------------------------------------------------
-- Type Alias Definition

alias_defn :: { AliasDefn }
alias_defn
  : alias_decl '=' type ';' { AliasDefn (locOf $1 <> $4) $1 $3}

alias_decl :: { AliasDecl }
alias_decl
  : Alias con_name maybe_scheme type_parameters { AliasDecl ($1 <> locOf $4) $2 $3 $4 }

-- -----------------------------------------------------------------------------
-- | Class Definition

class_defn :: { ClassDefn }
class_defn
  : class_decl class_body   { ClassDefn (locOf $1 <> locOf $2) $1 $2 }

class_decl :: { ClassDecl }
class_decl
  : Class con_name maybe_scheme type_parameters
    { ClassDecl ($1 <> locOf $4) $2 $3 $4 }


class_body :: { ClassBody }
class_body
  : '{' class_method_list0 '}' { ClassBody ($1 <> $3) $2 }

class_method :: { ClassMethod }
class_method
  : function_decl ';'   { ClassMethod (locOf $1 <> $2)       $1 Nothing   }
  | function_decl block { ClassMethod (locOf $1 <> locOf $2) $1 (Just $2) } 

class_method_list0 :: { [ClassMethod] }
class_method_list0
  : {- empty -}       { [] }
  | class_method_list { $1 }

class_method_list :: { [ClassMethod] }
class_method_list
  : class_method_list_r { reverse $1 }

class_method_list_r :: { [ClassMethod] }
class_method_list_r
  : class_method                     {    [$1] }
  | class_method_list_r class_method { $2 : $1 }

-- -----------------------------------------------------------------------------
-- | Instance Definition

instance_defn :: { InstDefn }
instance_defn
  : instance_decl instance_body { InstDefn (locOf $1 <> locOf $2) $1 $2 }


instance_decl :: { InstDecl }
instance_decl
  : Inst con_name maybe_scheme type_arguments { InstDecl ($1 <> locOf $4) $2 $3 $4 }


instance_body :: { InstBody }
instance_body
  : '{' instance_method_list0 '}' { InstBody ($1 <> $3) $2 }

instance_method :: { InstMethod }
instance_method
  : function_defn { InstMethod (locOf $1) $1 } 

instance_method_list0 :: { [InstMethod] }
instance_method_list0
  : {- empty -}          { [] }
  | instance_method_list { $1 }

instance_method_list :: { [InstMethod] }
instance_method_list
  : instance_method_list_r { reverse $1 }

instance_method_list_r :: { [InstMethod] }
instance_method_list_r
  : instance_method                        {    [$1] }
  | instance_method_list_r instance_method { $2 : $1 }

-- -----------------------------------------------------------------------------
-- | Operator Declaration

operator_decl :: { OpDecl }
operator_decl
  : Operator '(' fixity ',' integer ',' op_name_list ')' ';'
    { OpDecl ($1 <> $9) $3 (extractInteger $5) $7 }


fixity :: { Fixity }
fixity
  : Infix   { InfixN $1 }
  | Infixr  { InfixR $1 }
  | Infixl  { InfixL $1 }
  | Prefix  { Prefix $1 }
  | Postfix { Postfix $1 }

op_name_list :: { [Name] }
op_name_list : op_name_list_r { reverse $1 }

op_name_list_r :: { [Name] }
op_name_list_r
  : op_name                    {    [$1] }
  | op_name_list_r ',' op_name { $3 : $1 } 

{
parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError (tok:toks) =
  let locPrefix = (show . pretty . locOf $ tok)
      tokStr    = unpack (_tokText tok)
      tokClassStr = (show . pretty . _tokClass $ tok)
  in error $ "\n" ++ locPrefix ++ ": Parse error on " ++ tokStr ++ " (" ++ tokClassStr ++ ")"

}
