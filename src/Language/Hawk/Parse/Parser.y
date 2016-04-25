{
module Language.Hawk.Parse.Parser where

import Language.Hawk.Parse.Lexer
import Language.Hawk.Syntax.AST
import Language.Hawk.Data.Node

import Data.Monoid

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
    
    
    INT     { Token _ (TokenInt  _)     }
    FLOAT   { Token _ (TokenFloat  _)   }
    CHAR    { Token _ (TokenChar  _)    }
    STRING  { Token _ (TokenString  _)  }
    
    MOD       { Token _ TokenModule }
    USE       { Token _ TokenUse }
    USE_QUAL  { Token _ TokenUseQualified }
    
    PUB     { Token _ TokenPublic   }
    PRIV    { Token _ TokenPrivate  }
    LINK    { Token _ TokenLink }
    
    FN      { Token _ TokenFunction }
    VAL     { Token _ TokenValue    }
    VAR     { Token _ TokenVariable }
    
    DO      { Token _ TokenDo }
    RETURN  { Token _ TokenReturn }
    
    
    '::'    { Token _ TokenDblColon }
    
    ':='    { Token _ TokenFuncDef }
    ':-'    { Token _ TokenTypeDec }
    ':~'    { Token _ TokenTypeClass }
    ':+'    { Token _ TokenImplement }
    
    '<-'    { Token _ TokenLArrow }
    '<='    { Token _ TokenThickLArrow }
    '->'    { Token _ TokenRArrow }
    '=>'    { Token _ TokenThickRArrow }
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
    
    '('     { Token _ TokenLParen }
    ')'     { Token _ TokenRParen }
    '['     { Token _ TokenLBracket }
    ']'     { Token _ TokenRBracket }
    '{'     { Token _ TokenLCurlyBrace }
    '}'     { Token _ TokenRCurlyBrace }
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

trans_unit :: { HkTranslUnitNode }
  : root_mod { HkTranslUnit $1 (nodeInfo $1) }
  
root_mod :: { HkRootModuleNode }
  : MOD dotted_mod_id ext_stmts { HkRootModule $2 $3 (nodeInfo $1 <> nodesInfo $3)  }


-- -----------------------------------------------------------------------------
-- Hawk Parser "General"  
  
ty_id :: { HkIdentNode }
  : ID_CAP_USCORE           { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_CAP_USCORE_NUM_TICK  { HkIdent (getTokId $1) (nodeInfo $1) }
  
tyvar_id :: { HkIdentNode }
  : ID_LOWER                { HkIdent (getTokId $1) (nodeInfo $1) }

obj_id :: { HkIdentNode }
  : ID_LOWER                { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK      { HkIdent (getTokId $1) (nodeInfo $1) }

fn_id :: { HkIdentNode }
  : ID_LOWER                { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK      { HkIdent (getTokId $1) (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- | Hawk Parser "External Statments"

ext_stmt :: { HkExtStmtNode }
  : mod_dec       { $1 }
  | import_dec    { $1 }
  
ext_stmts :: { [HkExtStmtNode] }
  : ext_stmt                { [$1] }
  | ext_stmts ext_stmt      { $1 ++ [$2] }
  
ext_block :: { HkExtBlockNode }
  : '{' '}'                     { HkExtBlock [] (nodeInfo $1 <> nodeInfo $2) }
  | '{' ext_stmts '}'           { HkExtBlock $2 (nodeInfo $1 <> nodeInfo $3) }


  
vis_tag :: { HkVisibilityTagNode }
  : PUB                     { HkPublic  (nodeInfo $1) }
  | PRIV                    { HkPrivate (nodeInfo $1) }

-- -----------------------------------------------------------------------------
-- | Hawk Parser "Module"

mod_dec :: { HkExtStmtNode }
  : MOD dotted_mod_id ext_block { HkModDef (HkPublic (nodeInfo $1)) $2 $3 ((nodeInfo $1) <> (nodeInfo $3)) }
  | vis_tag MOD dotted_mod_id ext_block { HkModDef $1 $3 $4 ((nodeInfo $1) <> (nodeInfo $4)) }

mod_id :: { HkIdentNode }
  : ID_CAP_USCORE           { HkIdent (getTokId $1) (nodeInfo $1) }  
  
dotted_mod_id :: { HkDottedIdentNode }
  : mod_id                          { [$1] }
  | dotted_mod_id '.' mod_id        { $1 ++ [$3] }

-- -----------------------------------------------------------------------------
-- Hawk Parser "Import"
  
import_dec :: { HkExtStmtNode }
  : USE      import_items                   { HkExtImport     (HkPrivate (nodeInfo $1)) $2 (nodeInfo $1 <> nodesInfo $2) }
  | USE_QUAL import_items                   { HkExtImportQual (HkPrivate (nodeInfo $1)) $2 (nodeInfo $1 <> nodesInfo $2) }
  | vis_tag USE      import_items           { HkExtImport     $1 $3 (nodeInfo $1 <> nodesInfo $3) }
  | vis_tag USE_QUAL import_items           { HkExtImportQual $1 $3 (nodeInfo $1 <> nodesInfo $3) }

import_items :: { HkImportItemsNode }
  : import_item                             { [$1] }
  | dotted_mod_id '(' import_specs ')'      { prefixImportItems $1 $3 }

import_item :: { HkImportItemNode }
  : dotted_mod_id                           { HkImportItem $1 Nothing (nodesInfo $1) }
  | dotted_mod_id '.' import_target         { HkImportItem ($1 ++ [$3]) Nothing (nodesInfo $1 <> nodeInfo $3) }
  
import_specs :: { HkImportItemsNode }
  : import_spec                             { $1 }
  | import_specs ',' import_spec            { $1 ++ $3 }
  
import_spec :: { HkImportItemsNode }
  : import_target                           { [HkImportItem [$1] Nothing (nodeInfo $1)] }
  | import_items                            { $1 }
  
import_target :: { HkIdentNode }
  : ID_CAP_USCORE_NUM_TICK                  { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_USCORE_NUM_TICK                      { HkIdent (getTokId $1) (nodeInfo $1) }
  | ID_LOWER                                { HkIdent (getTokId $1) (nodeInfo $1) }


-- -----------------------------------------------------------------------------
-- Hawk Parser "Function"




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
