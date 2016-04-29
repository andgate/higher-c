{
module Language.Hawk.Parse.Parser where


import Data.Monoid

import Language.Hawk.Data.Node
import Language.Hawk.Syntax.AST
import Language.Hawk.Parse.Lexer
import Language.Hawk.Parse.Utils


}

%name parseHk
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEof }
%error { happyError }

%token
    MOD             { Token _ TokenModule }
    USE             { Token _ TokenUse }
    USE_QUAL        { Token _ TokenUseQualified }
    AS              { Token _ TokenAs }
    
    PUB             { Token _ TokenPublic   }
    PRIV            { Token _ TokenPrivate  }
    LINK            { Token _ TokenLink }
    
    DATA            { Token _ TokenData }
    ENUM            { Token _ TokenEnum }
    TYPE            { Token _ TokenType }
    CLASS           { Token _ TokenClass }
    INST            { Token _ TokenInst }
    FN              { Token _ TokenFunction }
    VAL             { Token _ TokenValue    }
    VAR             { Token _ TokenVariable }
    
    DO              { Token _ TokenDo }
    RETURN          { Token _ TokenReturn }
    
    IF              { Token _ TokenIf }
    THEN            { Token _ TokenThen}
    ELSE            { Token _ TokenElse }
    ELIF            { Token _ TokenElif }
    WHILE           { Token _ TokenWhile }
    FOR             { Token _ TokenFor }
    IN              { Token _ TokenIn }
    
    
    '()'            { Token _ TokenParenPair }
    BOOL_TY         { Token _ TokenBoolTy }
    W8_TY           { Token _ TokenW8Ty }
    W16_TY          { Token _ TokenW16Ty }
    W32_TY          { Token _ TokenW32Ty }
    W64_TY          { Token _ TokenW64Ty }
    I8_TY           { Token _ TokenI8Ty }
    I16_TY          { Token _ TokenI16Ty }
    I32_TY          { Token _ TokenI32Ty }
    I64_TY          { Token _ TokenI64Ty }
    F32_TY          { Token _ TokenF32Ty }
    F64_TY          { Token _ TokenF64Ty }
    CHAR_TY         { Token _ TokenCharTy }
    STRING_TY       { Token _ TokenStringTy }
    
    '@'             { Token _ TokenAtSign }
    '#'             { Token _ TokenPound }
    ':'             { Token _ TokenColon }
    '::'            { Token _ TokenDblColon }
    '_'             { Token _ TokenUnderscore }
    
    '<-'            { Token _ TokenLArrow }
    '->'            { Token _ TokenRArrow }
    '<='            { Token _ TokenThickLArrow }
    '=>'            { Token _ TokenThickRArrow }
    '<:'            { Token _ TokenLSubArrow }
    ':>'            { Token _ TokenRSubArrow }
    
    '('             { Token _ TokenLParen }
    ')'             { Token _ TokenRParen }
    '['             { Token _ TokenLBracket }
    ']'             { Token _ TokenRBracket }
    '{'             { Token _ TokenLCurlyBrace }
    '}'             { Token _ TokenRCurlyBrace }
    
    '.'             { Token _ TokenPeriod }
    ','             { Token _ TokenComma }
    
    '='             { Token _ TokenEquals }
    '*='            { Token _ TokenStarEquals }
    '/='            { Token _ TokenSlashEquals }
    '%='            { Token _ TokenPercentEquals }
    '+='            { Token _ TokenPlusEquals }
    '-='            { Token _ TokenMinusEquals }
    '<<='           { Token _ TokenShlEq }
    '>>='           { Token _ TokenShrEq }
    '&='            { Token _ TokenAmpersandEquals }
    '|='            { Token _ TokenBarEquals }
    '^='            { Token _ TokenCaretEquals }
    
    '*'             { Token _ TokenStar }
    '/'             { Token _ TokenSlash }
    '%'             { Token _ TokenPercent }
    '+'             { Token _ TokenPlus }
    '-'             { Token _ TokenMinus }
    '<<'            { Token _ TokenShl }
    '>>'            { Token _ TokenShr }
    '<'             { Token _ TokenLesser }
    '>'             { Token _ TokenGreater }
    '>='            { Token _ TokenGreaterEquals }
    '=='            { Token _ TokenDblEquals }
    '!='            { Token _ TokenExclaimEquals }
    '&'             { Token _ TokenAmpersand }
    '^'             { Token _ TokenCaret }
    '|'             { Token _ TokenBar }
    '&&'            { Token _ TokenAmpAmp }
    '||'            { Token _ TokenBarBar }
    
    '++'            { Token _ TokenDblPlus }
    '--'            { Token _ TokenDblMinus }
    '~'             { Token _ TokenTilde }
    '!'             { Token _ TokenExclaim }
    
    '\\'            { Token _ TokenBackslash }
    
    ID_LOWER                  { Token _ (TokenIdLower _) }
    ID_CAP_USCORE             { Token _ (TokenIdCapUscore _) }
    ID_USCORE_NUM_TICK        { Token _ (TokenIdUScoreNumTick _) }
    ID_CAP_USCORE_NUM_TICK    { Token _ (TokenIdCapUScoreNumTick _) }
    
    INTEGER         { Token _ (TokenInteger _) }
    DOUBLE          { Token _ (TokenDouble _) }
    CHAR            { Token _ (TokenChar _) }
    STRING          { Token _ (TokenString _) }
    
    OPEN_BLOCK      { Token _ TokenOpenBlock }
    CLOSE_BLOCK     { Token _ TokenCloseBlock }
    OPEN_STMT       { Token _ TokenOpenStmt }
    CLOSE_STMT      { Token _ TokenCloseStmt }

%%

trans_unit :: { HkTranslUnitNode }
  : root_mod                                { HkTranslUnit $1 (nodeInfo $1) }
  
root_mod :: { HkRootModuleNode }
  : MOD mod_path ext_stmts              { HkRootModule $2 $3 (nodeInfo $1 <> nodesInfo $3)  }


-- -----------------------------------------------------------------------------
-- | Hawk Parser "Identifiers"

modid :: { HkNameNode }
  : ID_CAP_USCORE                           { HkIdent (getTokId $1) (nodeInfo $1) }

mod_path :: { HkModPathNode }
  : dotted_modid                            { HkModPath $1 (nodesInfo $1) }

dotted_modid :: { [HkNameNode] }
  : modid                                   { [$1] }
  | dotted_modid '.' modid                  { $1 ++ [$3] }
  
varid :: { HkNameNode }
  : ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK                      { HkIdent (getTokId $1) (nodeInfo $1) }

qvarid :: { HkQNameNode }
  : varid                                   { HkUnQual $1 }
  | mod_path '.' varid                      { HkQual $1 $3 (nodeInfo $1 <> nodeInfo $3) }

conid :: { HkNameNode }
  : ID_CAP_USCORE                           { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_CAP_USCORE_NUM_TICK                  { HkIdent (getTokId $1) (nodeInfo $1) }

qconid :: { HkQNameNode }
  : conid                                   { HkUnQual $1 }
  | mod_path '.' conid                      { HkQual $1 $3 (nodeInfo $1 <> nodeInfo $3) }

import_target_id :: { HkNameNode }
  : ID_CAP_USCORE                           { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_CAP_USCORE_NUM_TICK                  { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK                      { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }

tyvarid :: { HkNameNode }
  : ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }
  
tyvarids :: { [HkNameNode] }
  : tyvarid                                 { [$1] }
  | tyvarids tyvarid                        { $1 ++ [$2] }

tyvarids0 :: { [HkNameNode] }
  : tyvarids                                { $1 }
  | {- empty -}                             { [] }

-- -----------------------------------------------------------------------------
-- | Hawk Parser "External Statments"

ext_block :: { HkExtBlockNode }
  : '{' '}'                                 { HkExtBlock [] (nodeInfo $1 <> nodeInfo $2) }
  | '{' ext_stmts '}'                       { HkExtBlock $2 (nodeInfo $1 <> nodeInfo $3) }
  
ext_stmts :: { [HkExtStmtNode] }
  : ext_stmt                                { [$1] }
  | ext_stmts ext_stmt                      { $1 ++ [$2] }

ext_stmt :: { HkExtStmtNode }
  : mod_dec                                 { $1 }
  | import_dec                              { $1 }
  | ext_fn_stmt                             { $1 }
  
vis_tag :: { HkVisibilityTagNode }
  : PUB                                     { HkPublic  (nodeInfo $1) }
  | PRIV                                    { HkPrivate (nodeInfo $1) }
  
vis_tag0 :: { HkVisibilityTagNode }
  : vis_tag                                 { $1 }
  | {- empty -}                             { HkPublic mempty }
  
ext_fn_stmt :: { HkExtStmtNode }
  : vis_tag0 LINK fn_dec                    { HkExtFnLink $1 $3 (nodeInfo $1 <> nodeInfo $2 <> nodeInfo $3) }
  | vis_tag0 fn_dec                         { HkExtFnDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag0 fn_def                         { HkExtFnDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | vis_tag0 val_def                        { HkExtValDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag0 var_dec                        { HkExtVarDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag0 var_def                        { HkExtVarDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | vis_tag0 type_def                       { HkExtTypeDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- | Hawk Parser "Module"

mod_dec :: { HkExtStmtNode }
  : MOD mod_path ':' ext_block              { HkModDef (HkPublic mempty) $2 $4 ((nodeInfo $1) <> (nodeInfo $4)) }
  | vis_tag MOD mod_path ':' ext_block      { HkModDef $1 $3 $5 ((nodeInfo $1) <> (nodeInfo $5)) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Import"

import_dec :: { HkExtStmtNode }
  : USE      import_path                    { HkExtImport     (HkPrivate (nodeInfo $1)) $2 (nodeInfo $1 <> nodeInfo $2) }
  | USE_QUAL import_path                    { HkExtImportQual (HkPrivate (nodeInfo $1)) $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag USE      import_path            { HkExtImport     $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | vis_tag USE_QUAL import_path            { HkExtImportQual $1 $3 (nodeInfo $1 <> nodeInfo $3) }

import_path :: { HkImportPathNode }
  : modid '.' import_path                   { HkImportPath $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | import_target import_alias0             { HkImportTarget $1 $2 (nodeInfo $1 <> maybeNodeInfo $2) }

import_target :: { HkImportTargetNode }
  : import_target_id                        { HkITarget $1 }
  | '(' import_paths ')'                    { HkIPaths $2 (nodeInfo $1 <> nodeInfo $3) }
  
import_paths :: { [HkImportPathNode] }
  : import_path                             { [$1] }
  | import_paths ',' import_path            { $1 ++ [$3] }


import_alias0 :: { Maybe HkNameNode }
  : import_alias                            { Just $1 }
  | {- empty -}                             { Nothing }

import_alias :: { HkNameNode }
  : AS modid                                { $2 }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Function"

fn_dec :: { HkFnDecNode }
  : FN varid '::' ctype                     { HkFnDec $2 $4 (nodeInfo $1 <> nodeInfo $4) }

fn_def :: { HkFnDefNode }
  : fn_dec bindings                         { HkFnDef $1 $2 (nodeInfo $1 <> nodesInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Object"

obj_dec :: { HkObjDecNode }
  : varid '::' type                         { HkObjDec $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  
obj_def :: { HkObjDefNode }
  : obj_dec '=' '{' exp '}'                 { HkObjDef $1 $4 (nodeInfo $1 <> nodeInfo $5) }
  
val_dec :: { HkValDecNode }
  : VAL obj_dec                             { HkValDec $2 (nodeInfo $1 <> nodeInfo $2) }

val_def :: { HkValDefNode }  
  : VAL obj_def                             { HkValDef $2 (nodeInfo $1 <> nodeInfo $2) }

var_dec :: { HkVarDecNode }
  : VAR obj_dec                             { HkVarDec $2 (nodeInfo $1 <> nodeInfo $2) }

var_def :: { HkVarDefNode }  
  : VAR obj_def                             { HkVarDef $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type Definition"

type_def :: { HkTypeDefNode }
  : alias_def                               { HkTyAliasDef $1 (nodeInfo $1) }
  | rec_def                                 { HkTyRecDef   $1 (nodeInfo $1) }
  | union_def                               { HkTyUnionDef $1 (nodeInfo $1) }
  | class_def                               { HkTyClassDef $1 (nodeInfo $1) }
  | inst_def                                { HkTyClassInstDef $1 (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type Alias"

alias_def :: { HkTypeAliasDefNode }
  : TYPE conid tyvarids0 rcontext0 '=' type
    { HkTypeAliasDef $2 $3 $4 $6 (nodeInfo $1 <> nodeInfo $6) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Record"

rec_def :: { HkRecordDefNode }
  : DATA conid tyvarids0 rsupertys0 rcontext0 rec_member_block 
    { HkRecordDef $2 $3 $4 $5 $6 (nodeInfo $1 <> nodeInfo $2 <> nodesInfo $3 <> nodesInfo $4 <> nodesInfo $5 <> nodesInfo $6) }
  
rec_member_block :: { [HkRecordMemberNode] }
  : '=' rec_members                         { $2 }     
  | {- empty -}                             { [] }
  
rec_members :: { [HkRecordMemberNode] }
  : rec_member                              { [$1] }
  | rec_members ',' rec_member              { $1 ++ [$3] }

rec_member :: { HkRecordMemberNode }
  : vis_tag0 val_def                        { HkRecordValDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag0 var_dec                        { HkRecordVarDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag0 var_def                        { HkRecordVarDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Union"

union_def :: { HkUnionDefNode }
  : ENUM conid tyvarids0 rcontext0 union_block
    { HkUnionDef $2 $3 $4 $5 (nodeInfo $1 <> nodesInfo $5) }
 
union_block :: { [HkUnionElementNode] }
  : '=' union_elems                        { $2 }
 
union_elems :: { [HkUnionElementNode] }
  : union_elem                              { [$1] }
  | union_elems '|' union_elem              { $1 ++ [$3] }
  
union_elem  :: { HkUnionElementNode }
  : conid                                   { HkUnionElement $1 [] (nodeInfo $1) }
  | conid union_elem_types                  { HkUnionElement $1 $2 (nodeInfo $1 <> nodesInfo $2) }
  
union_elem_types :: { [HkTypeNode] }  
  : union_elem_type                         { [$1] }
  | union_elem_types union_elem_type        { $1 ++ [$2] }
  
union_elem_type :: { HkTypeNode }
  : atype                                   { $1 }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Class"

class_def :: { HkClassDefNode }
  : CLASS conid tyvarids rcontext0 class_body
    { HkClassDef $2 $3 $4 $5 (nodeInfo $1 <> nodesInfo $5) }

class_body :: { [HkClassMemberNode] }
  : ':' '{' class_members '}'               { $3 }
  
class_members :: { [HkClassMemberNode] }
  : class_member                            { [$1] }
  | class_members class_member              { $1 ++ [$2] }
  
class_member :: { HkClassMemberNode }
  : vis_tag0 fn_dec                         { HkClassMemberDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag0 fn_def                         { HkClassMemberDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Class Instance"

inst_def :: { HkClassInstDefNode }
  : INST conid atypes rcontext0 inst_body
    { HkClassInstDef $2 $3 $4 $5 (nodeInfo $1 <> nodesInfo $5) }

inst_body :: { [HkClassInstMemberNode] }
  : ':' '{' inst_members '}'                { $3 }
  
inst_members :: { [HkClassInstMemberNode] }
  : inst_member                             { [$1] }
  | inst_members inst_member                { $1 ++ [$2] }
  
inst_member  :: { HkClassInstMemberNode }
  : vis_tag0 fn_def                         { HkClassInstMemberDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type"

ctype :: { HkQualTypeNode }
  : lcontext0 type                          { HkQualType $1 $2 (nodesInfo $1 <> nodeInfo $2) }

type :: { HkTypeNode }
  : btype '->' type                         { HkTyFun $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | btype                                   { $1 }
  
btype :: { HkTypeNode }
  : btype atype                             { HkTyApp $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | atype                                   { $1 }
  
atype :: { HkTypeNode }
  : typrim                                  { $1 }
  | tycon                                   { $1 }
  | tyvar                                   { $1 }
  | tyconst                                 { $1 }
  | tyref                                   { $1 }
  | tyarray                                 { $1 }
  | tytuple                                 { $1 }
  | tygroup                                 { $1 }

typrim :: { HkTypeNode }
  : prim_type                               { HkTyPrim $1 (nodeInfo $1) }

tycon :: { HkTypeNode }
  : qconid                                  { HkTyCon $1 (nodeInfo $1) }

tyvar :: { HkTypeNode }
  : tyvarid                                 { HkTyVar $1 (nodeInfo $1) }

tyconst :: { HkTypeNode }  
  : '#' atype                               { HkTyConst $2 (nodeInfo $1 <> nodeInfo $2) }

tyref :: { HkTypeNode }  
  : '*' atype                               { HkTyRef $2 (nodeInfo $1 <> nodeInfo $2) }

tyarray :: { HkTypeNode }
  : '[' type ']'                            { HkTyArray $2 (nodeInfo $1 <> nodeInfo $3) }

tytuple :: { HkTypeNode }
  : '(' types ')'                           { HkTyTuple $2 (nodeInfo $1 <> nodeInfo $3) }
  
tygroup :: { HkTypeNode }
  : '(' type ')'                            { $2 }

types :: { [HkTypeNode] }
  : type ',' type                           { [$1, $3] }
  | types ',' type                          { $1 ++ [$3] }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type Contexts"

rcontext0 :: { HkTypeContextNode }
  : rcontext                                { $1 }
  | {- empty -}                             { [] }

rcontext :: { HkTypeContextNode }
  : '<=' context                            { $2 }

lcontext0 :: { HkTypeContextNode }
  : lcontext                                { $1 }
  | {- empty -}                             { [] }
 
lcontext :: { HkTypeContextNode }
  : context '=>'                            { $1 }
  
context :: { HkTypeContextNode }
  : class_asst                              { [$1] }
  | context ',' class_asst                  { $1 ++ [$3] }

class_asst :: { HkClassAsstNode }
  : qconid atypes                           { HkClassAsst $1 $2 (nodeInfo $1 <> nodesInfo $2) }

atypes :: { [HkTypeNode] }
  : atype                                   { [$1] }
  | atypes atype                            { $1 ++ [$2] }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Super Types"

lsupertys0 :: { [HkTypeNode] }
  : lsupertys                               { $1 }
  | {- empty -}                             { [] }
  
lsupertys :: { [HkTypeNode] }
  : rsupertys ':>'                          { $1 }

rsupertys0 :: { [HkTypeNode] }
  : rsupertys                               { $1 }
  | {- empty -}                             { [] }
  
rsupertys :: { [HkTypeNode] }
  : '<:' supertys                           { $2 }
 
supertys :: { [HkTypeNode] }
  : superty                               { [$1] }
  | supertys ',' superty                  { $1 ++ [$3] }
  
superty :: { HkTypeNode }
  : type                                    { $1 }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type Identifiers"
  
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Primitive Type"

prim_type :: { HkPrimTypeNode }
  : '()'                                    { HkTyUnit (nodeInfo $1) }
  | BOOL_TY                                 { HkTyBool (nodeInfo $1) }
  | W8_TY                                   { HkTyW8 (nodeInfo $1) }
  | W16_TY                                  { HkTyW16 (nodeInfo $1) }
  | W32_TY                                  { HkTyW32 (nodeInfo $1) }
  | W64_TY                                  { HkTyW64 (nodeInfo $1) }
  | I8_TY                                   { HkTyI8 (nodeInfo $1) }
  | I16_TY                                  { HkTyI16 (nodeInfo $1) }
  | I32_TY                                  { HkTyI32 (nodeInfo $1) }
  | I64_TY                                  { HkTyI64 (nodeInfo $1) }
  | F32_TY                                  { HkTyF32 (nodeInfo $1) }
  | F64_TY                                  { HkTyF64 (nodeInfo $1) }
  | CHAR_TY                                 { HkTyChar (nodeInfo $1) }
  | STRING_TY                               { HkTyString (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Guarderd Pattern Bindings"

bindings :: { [HkBindingNode] }
  : binding                                 { [$1] }
  | bindings binding                        { $1 ++ [$2] }
  
binding :: { HkBindingNode }
  : '|' patterns binding_block              { HkBinding $2 $3 (nodeInfo $1 <> nodeInfo $3) }
  | binding_block                           { HkBinding [] $1 (nodeInfo $1) }

binding_block :: { HkBindingBlockNode }
  : ':' block                               { HkBindingBlock $2 (nodeInfo $1 <> nodeInfo $2) }
  | '=' '{' exp '}'                         { HkBindingExp $3 (nodeInfo $1 <> nodeInfo $4) }
  | guarded_binding_blocks                  { HkGuardedBindingBlock $1 (nodesInfo $1) }
  
guarded_binding_blocks :: { [HkGuardedBlockNode] }
  : guarded_binding_block                               { [$1] }
  | guarded_binding_blocks guarded_binding_block        { $1 ++ [$2] }
  
guarded_binding_block :: { HkGuardedBlockNode }
  : guard ':' block                         { HkGuardedBlock $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | guard '=' '{' exp '}'                   { HkGuardedExp $1 $4 (nodeInfo $1 <> nodeInfo $5) }
  
guard :: { HkGuardNode }
  : '*' exp                                 { HkGuardExp $2 (nodeInfo $1 <> nodeInfo $2) }
  | '*' '_'                                 { HkGuardAny (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Pattern"

patterns :: { [HkPatternNode] }
  : pattern                                 { [$1] }
  | patterns pattern                        { $1 ++ [$2] }

pattern :: { HkPatternNode }
  : varid                                   { HkPatIdent $1 (nodeInfo $1) }
  | const_obj                               { HkPatConst $1 (nodeInfo $1) }
  | qconid                                  { HkPatRec $1 [] (nodeInfo $1) }
  | '(' qconid patterns ')'                 { HkPatRec $2 $3 (nodeInfo $1 <> nodeInfo $4) }
  | '(' pattern_tuple ')'                   { HkPatTuple $2 (nodeInfo $1 <> nodeInfo $3) }
  | varid '@' pattern                       { HkPatAlias $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | '_'                                     { HkPatAny (nodeInfo $1) }
  
pattern_tuple :: { [HkPatternNode] }
  : pattern                                 { [$1] }
  | pattern_tuple ',' pattern               { $1 ++ [$3] }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Statement"

block :: { HkBlockNode }
  : '{' block_stmts '}'                     { HkBlock $2 (nodeInfo $1 <> nodeInfo $3) }
  
block_stmts :: { [HkBlockStmtNode] }
  : '{' block_stmt '}'                      { [$2] }
  | block_stmts '{' block_stmt '}'          { $1 ++ [$3] }

block_stmt :: { HkBlockStmtNode }
  : DO ':' block                            { HkStmtBlock $3 (nodeInfo $1 <> nodeInfo $3) }
  | fexp                                    { HkStmtExp $1 (nodeInfo $1) }
  
  | val_def                                 { HkStmtValDef $1 (nodeInfo $1) }
  | var_dec                                 { HkStmtVarDec $1 (nodeInfo $1) }
  | var_def                                 { HkStmtVarDef $1 (nodeInfo $1) }
  
  | qvarid '=' exp                          { HkStmtAssign $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  
  | RETURN exp                              { HkStmtReturn $2 (nodeInfo $1 <> nodeInfo $2) }
  
  -- Missing case statement
  
  | if_stmt                                 { $1 }
  
  | {- empty -}                             { HkStmtEmpty mempty }

if_stmt :: { HkBlockStmtNode }
  : IF exp ':' block else_stmt0             { HkStmtIf $2 $4 $5 (nodeInfo $1 <> nodeInfo $4 <> nodeInfo $5) } 

-- Else statements are the only statements that are chained.
-- Since statements are held in blocks, parenthesis are autogenerated.
-- So else statements MUST explicity open and close their statement lines.
else_stmt0 :: { HkBlockStmtNode }
  : '}' '{' ELIF exp ':' block else_stmt0   { HkStmtIf $4 $6 $7 (nodeInfo $1 <> nodeInfo $6 <> nodeInfo $7) }
  | '}' '{' ELSE ':' block                  { HkStmtElse $5 (nodeInfo $1 <> nodeInfo $5) }
  | {- empty -}                             { HkStmtEmpty mempty }
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Expression"

exp :: { HkExpNode }
  : exp0b '::' ctype                        { HkExpCast $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | exp0                                    { $1 }
  
exp0 :: { HkExpNode }
  : exp0a                                   { $1 }
  | exp0b                                   { $1 }
  
exp0a :: { HkExpNode }
  : exp0b bin_op exp10a                     { HkExpInfixApp $1 $2 $3 (nodeInfo $1 <> nodeInfo $3) }
  | exp10a                                  { $1 }
  
exp0b :: { HkExpNode }
  : exp0b bin_op exp10b                     { HkExpInfixApp $1 $2 $3 (nodeInfo $1 <> nodeInfo $3) }
  | exp10b                                  { $1 }
  
exp10a :: { HkExpNode }
  : '\\' patterns '->' exp                  { HkExpLambda $2 $4 (nodeInfo $1 <> nodeInfo $4) }
  | IF exp THEN exp ELSE exp                { HkExpIfThenElse $2 $4 $6 (nodeInfo $1 <> nodeInfo $6) }
  
exp10b :: { HkExpNode }
  : DO ':' block                            { HkExpDo $3 (nodeInfo $1 <> nodeInfo $3) }
  | fexp                                    { $1 }
  
fexp :: { HkExpNode }
  : fexp aexp                               { HkExpApp $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  | aexp                                    { $1 }
  
aexp :: { HkExpNode }
  : const_obj                               { HkConstExp $1 }
  | qvarid                                  { HkExpVar $1 }
  | qconid                                  { HkExpCon $1 }
  | '(' exp ')'                             { $2 }
  | prefix_op aexp                          { HkExpPrefixApp $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Operators"  

assign_op :: { HkAssignOpNode }
  : '='                                     { HkAssignOp (nodeInfo $1) }
  | '*='                                    { HkMulAssOp (nodeInfo $1) }
  | '/='                                    { HkDivAssOp (nodeInfo $1) }
  | '%='                                    { HkRmdAssOp (nodeInfo $1) }
  | '+='                                    { HkAddAssOp (nodeInfo $1) }
  | '-='                                    { HkSubAssOp (nodeInfo $1) }
  | '<<='                                   { HkShlAssOp (nodeInfo $1) }
  | '>>='                                   { HkShrAssOp (nodeInfo $1) }
  | '&='                                    { HkAndAssOp (nodeInfo $1) }
  | '^='                                    { HkXorAssOp (nodeInfo $1) }
  | '|='                                    { HkOrAssOp (nodeInfo $1) }

bin_op :: { HkBinaryOpNode }
  : '*'                                     { HkMulOp (nodeInfo $1) }
  | '/'                                     { HkDivOp (nodeInfo $1) }
  | '%'                                     { HkRmdOp (nodeInfo $1) }
  | '+'                                     { HkAddOp (nodeInfo $1) }
  | '-'                                     { HkSubOp (nodeInfo $1) }
  | '<<'                                    { HkShlOp (nodeInfo $1) }
  | '>>'                                    { HkShrOp (nodeInfo $1) }
  | '<'                                     { HkLeOp (nodeInfo $1) }
  | '>'                                     { HkGrOp (nodeInfo $1) }
  | '<='                                    { HkLeqOp (nodeInfo $1) }
  | '>='                                    { HkGeqOp (nodeInfo $1) }
  | '=='                                    { HkEqOp (nodeInfo $1) }
  | '!='                                    { HkNeqOp (nodeInfo $1) }
  | '&'                                     { HkAndOp (nodeInfo $1) }
  | '^'                                     { HkXorOp (nodeInfo $1) }
  | '|'                                     { HkOrOp (nodeInfo $1) }
  | '&&'                                    { HkLndOp (nodeInfo $1) }
  | '||'                                    { HkLorOp (nodeInfo $1) }

prefix_op :: { HkUnaryOpNode }
  : '++'                                    { HkPreIncOp (nodeInfo $1) }
  | '--'                                    { HkPreDecOp (nodeInfo $1) }
  | '&'                                     { HkAddrOp (nodeInfo $1) }
  | '*'                                     { HkIndOp (nodeInfo $1) }
  | '+'                                     { HkPlusOp (nodeInfo $1) }
  | '-'                                     { HkMinOp (nodeInfo $1) }
  | '~'                                     { HkCompOp (nodeInfo $1) }
  | '!'                                     { HkNegOp (nodeInfo $1) }
  
postfix_op :: { HkUnaryOpNode }
  : '++'                                    { HkPostIncOp (nodeInfo $1) }
  | '--'                                    { HkPostDecOp (nodeInfo $1) }
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Constant Object"

const_obj :: { HkConstNode }
  : '()'                                    { HkUnit (nodeInfo $1) }
  | INTEGER                                 { HkI64 (getTokInt $1) (nodeInfo $1) }
  | DOUBLE                                  { HkF64 (getTokRat $1) (nodeInfo $1) }
  | CHAR                                    { HkChar (getTokChar $1) (nodeInfo $1) }
  | STRING                                  { HkString (getTokStr $1) (nodeInfo $1) }

{


getTokId (Token _ (TokenIdLower s))            = s
getTokId (Token _ (TokenIdCapUscore s))        = s
getTokId (Token _ (TokenIdUScoreNumTick s))    = s
getTokId (Token _ (TokenIdCapUScoreNumTick s)) = s

getTokInt   (Token _ (TokenInteger i))  = i
getTokRat   (Token _ (TokenDouble d))   = d
getTokChar  (Token _ (TokenChar c))     = c
getTokStr   (Token _ (TokenString s))   = s

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError tok@(Token (TokenInfo n p _) t) =
  alexError' p ("parse error at token '" ++ show t ++ "'" ++ "\n" ++ show tok)

parse :: FilePath -> String -> Either String HkTranslUnitNode
parse = runAlex' parseHk

parseFile :: FilePath -> IO (Either String HkTranslUnitNode)
parseFile p = readFile p >>= return . parse p


}
