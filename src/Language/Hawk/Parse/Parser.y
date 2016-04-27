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
    ID_LOWER                  { Token _ (TokenIdLower _) }
    ID_CAP_USCORE             { Token _ (TokenIdCapUscore _) }
    ID_USCORE_NUM_TICK        { Token _ (TokenIdUScoreNumTick _) }
    ID_CAP_USCORE_NUM_TICK    { Token _ (TokenIdCapUScoreNumTick _) }
    
    
    INT             { Token _ (TokenInt  _)     }
    FLOAT           { Token _ (TokenFloat  _)   }
    CHAR            { Token _ (TokenChar  _)    }
    STRING          { Token _ (TokenString  _)  }
    
    MOD             { Token _ TokenModule }
    USE             { Token _ TokenUse }
    USE_QUAL        { Token _ TokenUseQualified }
    AS              { Token _ TokenAs }
    
    PUB             { Token _ TokenPublic   }
    PRIV            { Token _ TokenPrivate  }
    LINK            { Token _ TokenLink }
    
    TY              { Token _ TokenType }
    FN              { Token _ TokenFunction }
    VAL             { Token _ TokenValue    }
    VAR             { Token _ TokenVariable }
    
    DO              { Token _ TokenDo }
    RETURN          { Token _ TokenReturn }
    
    '()'            { Token _ TokenParenPair }
    
    BIT_TY          { Token _ TokenBitTy }
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
    
    ':'             { Token _ TokenColon }
    '::'            { Token _ TokenDblColon }
    
    
    '|'             { Token _ TokenBar }
    '*'             { Token _ TokenStar }
    '_'             { Token _ TokenUnderscore }
    
    ':='            { Token _ TokenFuncDef }
    '='             { Token _ TokenEquals }
    
    ':-'            { Token _ TokenTypeDec }
    ':~'            { Token _ TokenTypeClass }
    ':+'            { Token _ TokenImplement }
    
    '<-'            { Token _ TokenLArrow }
    '<='            { Token _ TokenThickLArrow }
    '->'            { Token _ TokenRArrow }
    '=>'            { Token _ TokenThickRArrow }
    '<:'            { Token _ TokenSubtype }
    
    '`'             { Token _ TokenGrave }
    '~'             { Token _ TokenTilde }
    '!'             { Token _ TokenExclaim }
    '?'             { Token _ TokenQuestion }
    '@'             { Token _ TokenAt }
    '#'             { Token _ TokenPound }
    '$'             { Token _ TokenDollar }
    '%'             { Token _ TokenPercent }
    '^'             { Token _ TokenCaret }
    '&'             { Token _ TokenAmpersand }
    
    '('             { Token _ TokenLParen }
    ')'             { Token _ TokenRParen }
    '['             { Token _ TokenLBracket }
    ']'             { Token _ TokenRBracket }
    '{'             { Token _ TokenLCurlyBrace }
    '}'             { Token _ TokenRCurlyBrace }
    
    ';'             { Token _ TokenSemicolon }
    '.'             { Token _ TokenPeriod }
    ','             { Token _ TokenComma }
    '<'             { Token _ TokenLesser }
    '>'             { Token _ TokenGreater }
    
    '/'             { Token _ TokenSlash }
    '+'             { Token _ TokenPlus }
    '-'             { Token _ TokenMinus }
    
    OPEN_BLOCK      { Token _ TokenOpenBlock }
    CLOSE_BLOCK     { Token _ TokenCloseBlock }
    OPEN_STMT       { Token _ TokenOpenStmt }
    CLOSE_STMT      { Token _ TokenCloseStmt }

%%

trans_unit :: { HkTranslUnitNode }
  : root_mod                                { HkTranslUnit $1 (nodeInfo $1) }
  
root_mod :: { HkRootModuleNode }
  : MOD dotted_mod_id ext_stmts             { HkRootModule $2 $3 (nodeInfo $1 <> nodesInfo $3)  }

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
  
ext_fn_stmt :: { HkExtStmtNode }
  : LINK fn_dec                             { HkExtFnLink (HkPublic mempty) $2 (nodeInfo $1 <> nodeInfo $2) }
  | vis_tag LINK fn_dec                     { HkExtFnLink $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  
  | fn_dec                                  { HkExtFnDec (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag fn_dec                          { HkExtFnDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | fn_def                                  { HkExtFnDef (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag fn_def                          { HkExtFnDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | val_def                                 { HkExtValDef (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag val_def                         { HkExtValDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | var_dec                                 { HkExtVarDec (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag var_dec                         { HkExtVarDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | var_def                                 { HkExtVarDef (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag var_def                         { HkExtVarDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | type_def                                { HkExtTypeDef (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag type_def                        { HkExtTypeDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- | Hawk Parser "Module"

mod_dec :: { HkExtStmtNode }
  : MOD dotted_mod_id ':' ext_block         { HkModDef (HkPublic mempty) $2 $4 ((nodeInfo $1) <> (nodeInfo $4)) }
  | vis_tag MOD dotted_mod_id ':' ext_block { HkModDef $1 $3 $5 ((nodeInfo $1) <> (nodeInfo $5)) }

mod_id :: { HkIdentNode }
  : ID_CAP_USCORE                           { HkIdent (getTokId $1) (nodeInfo $1) }  
  
dotted_mod_id :: { HkDottedIdentNode }
  : mod_id                                  { [$1] }
  | dotted_mod_id '.' mod_id                { $1 ++ [$3] }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Import"
  
import_dec :: { HkExtStmtNode }
  : USE      import_items                   { HkExtImport     (HkPrivate (nodeInfo $1)) $2 (nodeInfo $1 <> nodesInfo $2) }
  | USE_QUAL import_items                   { HkExtImportQual (HkPrivate (nodeInfo $1)) $2 (nodeInfo $1 <> nodesInfo $2) }
  | vis_tag USE      import_items           { HkExtImport     $1 $3 (nodeInfo $1 <> nodesInfo $3) }
  | vis_tag USE_QUAL import_items           { HkExtImportQual $1 $3 (nodeInfo $1 <> nodesInfo $3) }

import_items :: { HkImportItemsNode }
  : import_item                                   { [$1] }
  | '(' import_items_list ')'                     { $2 }
  | '(' import_items_list ')' AS mod_id           { importItemsAlias (Just $5) $2 }
  | dotted_mod_id '(' import_specs ')'            { prefixImportItems $1 $3 }
  | dotted_mod_id '(' import_specs ')' AS mod_id  { prefixImportItemsAlias $1 (Just $6) $3 }

import_items_list :: { HkImportItemsNode }
  : import_items                                { $1 }
  | import_items_list import_items              { $1 ++ $2}

import_item :: { HkImportItemNode }
  : dotted_mod_id                               { HkImportItem $1 Nothing (nodesInfo $1) }
  | dotted_mod_id AS mod_id                     { HkImportItem $1 (Just $3) (nodesInfo $1 <> nodeInfo $3) }
  | dotted_mod_id '.' import_target             { HkImportItem ($1 ++ [$3]) Nothing (nodesInfo $1 <> nodeInfo $3) }
  | dotted_mod_id '.' import_target AS mod_id   { HkImportItem ($1 ++ [$3]) (Just $5) (nodesInfo $1 <> nodeInfo $5) }
  
import_specs :: { HkImportItemsNode }
  : import_spec                             { $1 }
  | import_specs import_spec                { $1 ++ $2 }
  
import_spec :: { HkImportItemsNode }
  : import_target                           { [HkImportItem [$1] Nothing (nodeInfo $1)] }
  | import_target AS mod_id                 { [HkImportItem [$1] (Just $3) (nodeInfo $1 <> nodeInfo $3)] }
  | import_items                            { $1 }
  
import_target :: { HkIdentNode }
  : ID_CAP_USCORE_NUM_TICK                  { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK                      { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Function"

fn_dec :: { HkFnDecNode }
  : FN fn_id '::' ctype                      { HkFnDec (HkSymIdent $2 (nodeInfo $2)) $4 (nodeInfo $1 <> nodeInfo $4) }

fn_def :: { HkFnDefNode }
  : fn_dec bindings                         { HkFnDef $1 $2 (nodeInfo $1 <> nodesInfo $2) }

fn_id :: { HkIdentNode }
  : ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK                      { HkIdent (getTokId $1) (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Object"

obj_dec :: { HkObjDecNode }
  : obj_id '::' type                        { HkObjDec $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  
obj_def :: { HkObjDefNode }
  : obj_dec '=' exp                         { HkObjDef $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  
val_dec :: { HkValDecNode }
  : VAL obj_dec                             { HkValDec $2 (nodeInfo $1 <> nodeInfo $2) }

val_def :: { HkValDefNode }  
  : VAL obj_def                             { HkValDef $2 (nodeInfo $1 <> nodeInfo $2) }

var_dec :: { HkVarDecNode }
  : VAR obj_dec                             { HkVarDec $2 (nodeInfo $1 <> nodeInfo $2) }

var_def :: { HkVarDefNode }  
  : VAR obj_def                             { HkVarDef $2 (nodeInfo $1 <> nodeInfo $2) }


obj_id :: { HkIdentNode }
  : ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK                      { HkIdent (getTokId $1) (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type"

type_def :: { HkTypeDefNode }
  : TY rec_def                              { HkTyRecDef $2 (nodeInfo $1 <> nodeInfo $2) }
  | TY union_def                            { HkTyUnionDef $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Record"

rec_def :: { HkRecordDefNode }
  : ty_id ':-' rec_member_block             { HkRecordDef $1 [] [] [] $3 (nodeInfo $1 <> nodesInfo $3) }
  
rec_member_block :: { [HkRecordMemberNode] }
  : rec_members                             { $1 }        
  
rec_members :: { [HkRecordMemberNode] }
  : rec_member                              { [$1] }
  | rec_members ',' rec_member              { $1 ++ [$3] }

rec_member :: { HkRecordMemberNode }
  : val_def                                 { HkRecordValDef (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag val_def                         { HkRecordValDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | var_dec                                 { HkRecordVarDec (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag var_dec                         { HkRecordVarDec $1 $2 (nodeInfo $1 <> nodeInfo $2) }
  
  | var_def                                 { HkRecordVarDef (HkPublic mempty) $1 (nodeInfo $1) }
  | vis_tag var_def                         { HkRecordVarDef $1 $2 (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Union"

union_def :: { HkUnionDefNode }
  : ty_id ':-' union_block                          { HkUnionDef $1 [] [] $3 (nodeInfo $1 <> nodesInfo $3) }
  | ty_id tyvar_ids ':-' union_block                { HkUnionDef $1 [] $2 $4 (nodeInfo $1 <> nodesInfo $4) }
  | ty_id tyvar_ids '<=' context ':-' union_block   { HkUnionDef $1 $4 $2 $6 (nodeInfo $1 <> nodesInfo $6) }
  
union_block :: { [HkUnionElementNode] }
  : union_elem                              { [$1] }
  | union_block '|' union_elem              { $1 ++ [$3] }
  
union_elem  :: { HkUnionElementNode }
  : ty_id                                   { HkUnionElement $1 [] (nodeInfo $1) }
  | ty_id union_elem_types                  { HkUnionElement $1 $2 (nodeInfo $1 <> nodesInfo $2) }
  
union_elem_types :: { [HkTypeNode] }  
  : union_elem_type                         { [$1] }
  | union_elem_types union_elem_type        { $1 ++ [$2] }
  
union_elem_type :: { HkTypeNode }
  : atype                                   { $1 }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type Alias"

-- -----------------------------------------------------------------------------
-- Hawk Parser "Class"

-- -----------------------------------------------------------------------------
-- Hawk Parser "Class Instance"

-- -----------------------------------------------------------------------------
-- Hawk Parser "Type"

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
  : dotted_ty_id                            { HkTyCon $1 (nodesInfo $1) }

tyvar :: { HkTypeNode }
  : tyvar_id                                { HkTyVar $1 (nodeInfo $1) }

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
  
atypes :: { [HkTypeNode] }
  : atype ',' atype                           { [$1, $3] }
  | atypes ',' atype                          { $1 ++ [$3] }


ctype :: { HkQualTypeNode }
  : type                                    { HkQualType [] $1 (nodeInfo $1) }
  | context '=>' type                       { HkQualType $1 $3 (nodesInfo $1 <> nodeInfo $3) }
  
context :: { HkTypeContextNode }
  : types                                   { checkContext $1 }

ty_id :: { HkIdentNode }
  : ID_CAP_USCORE                           { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_CAP_USCORE_NUM_TICK                  { HkIdent (getTokId $1) (nodeInfo $1) }
  
dotted_ty_id :: { HkDottedIdentNode }
  : ty_id                                   { [$1] }
  | dotted_mod_id '.' ty_id                 { $1 ++ [$3] }


tyvar_id :: { HkIdentNode }
  : ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }
  
tyvar_ids :: { [HkIdentNode] }
  : tyvar_id                                { [$1] }
  | tyvar_ids tyvar_id                      { $1 ++ [$2] }
  
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Primitive Type"

prim_type :: { HkPrimTypeNode }
  : '()'                                    { HkTyUnit (nodeInfo $1) }
  | BIT_TY                                  { HkTyBit (nodeInfo $1) }
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


-- -----------------------------------------------------------------------------
-- Hawk Parser "Guarderd Pattern Bindings"

bindings :: { [HkBindingNode] }
  : binding                                 { [$1] }
  | bindings binding                        { $1 ++ [$2] }
  
binding :: { HkBindingNode }
  : '|' patterns binding_block              { HkBinding $2 $3 (nodeInfo $1 <> nodeInfo $3) }
  | binding_block                           { HkBinding [] $1 (nodeInfo $1) }

binding_block :: { HkBindingBlockNode }
  : ':=' block                              { HkBindingBlock $2 (nodeInfo $1 <> nodeInfo $2) }
  | '=' exp                                 { HkBindingExp $2 (nodeInfo $1 <> nodeInfo $2) }
  | guarded_binding_blocks                  { HkGuardedBindingBlock $1 (nodesInfo $1) }
  
guarded_binding_blocks :: { [HkGuardedBlockNode] }
  : guarded_binding_block                               { [$1] }
  | guarded_binding_blocks guarded_binding_block        { $1 ++ [$2] }
  
guarded_binding_block :: { HkGuardedBlockNode }
  : guard ':=' block                        { HkGuardedBlock $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  | guard '=' exp                           { HkGuardedExp $1 $3 (nodeInfo $1 <> nodeInfo $3) }
  
guard :: { HkGuardNode }
  : '*' exp                                 { HkGuardExp $2 (nodeInfo $1 <> nodeInfo $2) }
  | '*' '_'                                 { HkGuardAny (nodeInfo $1 <> nodeInfo $2) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Pattern"

patterns :: { [HkPatternNode] }
  : pattern                                 { [$1] }
  | patterns pattern                        { $1 ++ [$2] }

pattern :: { HkPatternNode }
  : obj_id                                  { HkPatIdent $1 (nodeInfo $1) }
  | const_obj                               { HkPatConst $1 (nodeInfo $1) }
  | dotted_ty_id                            { HkPatRec $1 [] (nodesInfo $1) }
  | '(' dotted_ty_id patterns ')'           { HkPatRec $2 $3 (nodeInfo $1 <> nodeInfo $4) }
  | '(' pattern_tuple ')'                   { HkPatTuple $2 (nodeInfo $1 <> nodeInfo $3) }
  | obj_id '@' pattern                      { HkPatAlias $1 $3 (nodeInfo $1 <> nodeInfo $3) }
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
  : DO block                                { HkStmtBlock $2 (nodeInfo $1 <> nodeInfo $2) }
  | exp                                     { HkStmtExp $1 (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Expression"

exp :: { HkExpNode }
  : const_obj                               { HkConstExp $1 (nodeInfo $1) }
  
-- -----------------------------------------------------------------------------
-- Hawk Parser "Constant Object"

const_obj :: { HkConstNode }
  : '()'                                    { HkUnit (nodeInfo $1) }

{


getTokId (Token _ (TokenIdLower s))            = s
getTokId (Token _ (TokenIdCapUscore s))        = s
getTokId (Token _ (TokenIdUScoreNumTick s))    = s
getTokId (Token _ (TokenIdCapUScoreNumTick s)) = s

getTokInt     (Token _ (TokenInt s))    = s
getTokString  (Token _ (TokenString s)) = s

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
