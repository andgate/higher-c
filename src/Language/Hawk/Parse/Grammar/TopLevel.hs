{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar.TopLevel where

import Control.Applicative
import Data.Monoid
import Data.Tree
import Language.Hawk.Parse.Helpers
import Text.Earley


import qualified Language.Hawk.Parse.Lexer as Lex

import qualified Language.Hawk.Syntax.ClassDefinition as CD
import qualified Language.Hawk.Syntax.ClassInstance as CI
import qualified Language.Hawk.Syntax.ExpressionDefinition as ED
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.TaggedUnion as TU
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Lex.Token Lex.Token M.Source)
toplevel = mdo

-- -----------------------------------------------------------------------------
-- Module Rules
    modl <- rule $ M.Module "" <$> items <* eof


-- -----------------------------------------------------------------------------
-- Literal Rules
    lit <- rule $
            ( Lit.IntNum <$> tInteger )
        <|> ( Lit.FloatNum <$> tReal )
        <|> ( Lit.Chr <$> tChar )
        <|> ( Lit.Str <$> tString )
        <|> ( Lit.Boolean <$> tBool )
    

-- -----------------------------------------------------------------------------
-- Name rules
    varName <- rule $ varId
    
    conName <- rule $ conId
    
    opName <- rule $ opId
    
    name <- rule $ varName <|> conName


-- -----------------------------------------------------------------------------
-- Module path rules
    paths <- rule $
        Node N.empty <$> paths_subs
        
    paths_subs <- rule $  
        sep (rsvp ",") paths_base
        
    paths_base <- rule $
          (Node <$> conName <*> paths_sub)
      <|> (Node <$> varName <*> pure [])
    
    paths_sub <- rule $  
          ((:[]) <$> (op "." *> paths_base))
      <|> (rsvp "(" *> paths_subs <* rsvp ")")
      <|> (pure [])


-- -----------------------------------------------------------------------------
-- Item Rules
    items <- rule $
      linefolds item

    item <- rule $
          impItem
      <|> expItem
      <|> exprDefItem
      <|> typeDefItem
      <|> recordItem
      <|> taggedUnionItem
      <|> classDefItem
      <|> classInstItem
    
    
    impItem <- rule $
      I.Import <$> (op "->" *> paths)
      
    expItem <- rule $
      I.Export <$> (op "<-" *> paths)
      
    exprDefItem <- rule $
      I.ExprDef <$> exprDef
       
    typeDefItem <- rule $
      I.TypeDef <$> typeDef
      
    recordItem <- rule $
      I.Record <$> record
      
    taggedUnionItem <- rule $
      I.TaggedUnion <$> taggedUnion
      
    classDefItem <- rule $
      I.ClassDef <$> classDef
      
    classInstItem <- rule $
      I.ClassInst <$> classInst

-- -----------------------------------------------------------------------------
-- Type Signature Rules
    exprDefTypeSig <- rule $
          rsvp "?" *> tillEquals
          
    rawTypeSig <- rule $
          rsvp "?" *> raw
  
-- -----------------------------------------------------------------------------
-- Expression Definition Rules
    exprDef <- rule $
        exprDef0 <|> exprDef1
    
    exprDef0 <- rule $
        ED.ExprDef <$> opInfo0 <*> exprDefName <*> exprDefTypeSig <*> exprDefBody
       
    exprDef1 <- rule $
        ED.mkExprDef <$> opInfo0 <*> exprDefName <*> some varName <*> exprDefTypeSig <*> exprDefBody
    
    exprDefName <- rule $
        varName <|> parens opName
    
    exprDefOp <- rule $
        rsvp "=" <|> rsvp ":="
     
    exprDefBody <- rule $
        exprDefOp *> raw
    
        
    opInfo0 <- rule $
        opInfo <|> pure OI.defOpInfo
    
    opInfo <- rule $
        parens (OI.OpInfo <$> tInteger <*> (OI.assocFromName <$> varId))


-- -----------------------------------------------------------------------------
-- Type Definition Rules

    typeDef <- rule $
      TD.mkTypeDef <$> conName <*> many varName <*> (rsvp "=" *> raw)


-- -----------------------------------------------------------------------------
-- Declaration Rules    
    varDeclTyped <- rule $
      (,) <$> varName <*> rawTypeSig

    conDecl <- rule $
      (,) <$> conName <*> raw 
      
    conDeclTyped <- rule $
      (,) <$> conName <*> rawTypeSig  
    
-- -----------------------------------------------------------------------------
-- Record Rules

    record <- rule $
      R.mkRecord <$> conName <*> recordBody
    
    recordBody <- rule $
      rsvp ":" *> block varDeclTyped
    
    
-- -----------------------------------------------------------------------------
-- TaggedUnion Rules

    taggedUnion <- rule $
      TU.mkTaggedUnion <$> conName <*> tuBody
      
    
    tuBody <- rule $
      rsvp ":" *> block tuArm
      
    tuArm <- rule $
      rsvp "|" *> (conDecl <|> conDeclTyped)
    
    
-- -----------------------------------------------------------------------------
-- Class Definition Rules

    classDef <- rule $
      CD.mkClassDef <$> conName <*> some varName <*> classDefBody
    
    classDefBody <- rule $
      rsvp ":" *> block varDeclTyped
    
    
-- -----------------------------------------------------------------------------
-- Class Instance Rules
    
    classInst <- rule $
      CI.mkClassInst <$> typeCtx <*> conName <*> some varName <*> classInstBody
    
    classInstVar <- rule $
      raw
    
    classInstBody <- rule $
      rsvp ":" *> block exprDef
      
    typeCtx <- rule $
      many notTypeCtxArr *> rsvp "=>"


{- Types are parsed using a different grammar
-- -----------------------------------------------------------------------------
-- Type expressions
    typesig0 <- rule $
      (Just <$> typesig) <|> pure Nothing
    
    typesig <- rule $ rsvp "::" *> typ
    
    typ <- mixfixExpressionSeparate typOps typ'
    
    typ' <- rule $ parens typApp <|> typApp
    
    typApp <- rule $ 
      Ty.apply <$> typVal <*> many typVal
    
    typVal <- rule $ tyTuple <|> typCon <|> typUnit
      
    tyTuple <- rule $ Ty.tuple <$> (parens $ sep (rsvp ",") typApp)
        
    typCon <- rule $ Ty.Con <$> conName
    
    typUnit <- rule $ rsvp "(" *> rsvp ")" *> pure Ty.unit
-}


{- Expressions are another type of parser
-- -----------------------------------------------------------------------------
-- Statement Rules   
    stmtblock <- rule $ block statement
    
    statement <- rule $
          stmtRet
      <|> stmtCall
      <|> stmtAssign
      <|> stmtVarBind
      
    stmtCall <- rule $ 
      Stmt.Call <$> fexpr
    
    stmtVarBind <- rule $ 
      Stmt.Let <$> var
    
    stmtAssign <- rule $
      Stmt.Assign <$> varName <*> typesig0 <*> (op "=" *> expr)
    
    stmtRet <- rule $
      Stmt.Return <$> (rsvp "return" *> expr)
    

-- -----------------------------------------------------------------------------
-- Expression Rules
    expr <- mixfixExpressionSeparate exprOps expr'

    expr' <- rule $
          exprTyped
      <|> expr0
    
    
    exprTyped <- rule $
      E.Cast <$> expr0 <*> typesig
    
    
    expr0 <- rule $
          fexpr
      <|> aexpr
    
    
    fexpr <- rule $
        E.App <$> bexpr <*> many aexpr
    
    aexpr <- rule $
          litExpr
      <|> bexpr
      
    bexpr <- rule $ 
          varExpr
      <|> conExpr
      <|> nestedExpr
    
    varExpr <- rule $ E.Var <$> varName
    conExpr <- rule $ E.Con <$> conName
    litExpr <- rule $ E.Lit <$> lit
    nestedExpr <- rule $ parens expr
    
-}
    
    
    return modl