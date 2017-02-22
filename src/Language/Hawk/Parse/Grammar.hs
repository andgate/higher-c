{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Monoid
import Data.Tree
import Language.Hawk.Parse.Helpers
import Text.Earley
import Text.Earley.Mixfix


import qualified Language.Hawk.Parse.Lexer as L
import qualified Language.Hawk.Syntax.Alias as A
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.Function as F
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Ty
import qualified Language.Hawk.Syntax.Variable as V


-- -----------------------------------------------------------------------------
-- Grammar for Hawk
grammar :: TypeOpTable
        -> ExprOpTable
        -> Grammar r (Prod r L.Token L.Token M.Source)
grammar typOps exprOps = mdo

-- -----------------------------------------------------------------------------
-- Module Rules
    modl <- rule $ M.Module "" <$> items <* eof


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
      <|> aliasItem
      <|> recordItem
      <|> fnItem
      <|> varItem
    
    
    impItem <- rule $
      I.Import <$> (op "->" *> paths)
      
    expItem <- rule $
      I.Export <$> (op "<-" *> paths)
      
    fnItem <- rule $
      I.Function <$> function
       
    varItem <- rule $
      I.Variable <$> var
      
    recordItem <- rule $
      I.Record <$> record
      
    aliasItem <- rule $
      I.Alias <$> typAlias
    
-- -----------------------------------------------------------------------------
-- Binding rules  
    binding <- rule $
        B.Binding <$> bindMode <*> varName
    
    bindMode <- rule $
        byRef <|> byVal
      
    byVal <- rule $
        B.ByVal <$> mutability
    
    byRef <- rule $
        op "&" *>
        (pure B.ByRef <*> mutability)
      
    mutability <- rule $
        immutable <|> mutable
      
    immutable <- rule $
        op "!" *>
        pure B.Immutable
    
    mutable <- rule $
        pure B.Mutable
        

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

-- -----------------------------------------------------------------------------
-- Type Alias Rules
    typAlias <- rule $
      A.Alias <$> (conName <* rsvp "=") <*> typ

-- -----------------------------------------------------------------------------
-- Record Rules
    record <- rule $
      R.Record <$> (conName <* rsvp ":-") <*> record_fields
    
    record_fields <- rule $
      block record_field
    
    record_field <- rule $
      R.RecordField <$> varName <*> typesig

-- -----------------------------------------------------------------------------
-- Literal Rules
    lit <- rule $
            ( Lit.IntNum <$> tInteger )
        <|> ( Lit.FloatNum <$> tReal )
        <|> ( Lit.Chr <$> tChar )
        <|> ( Lit.Str <$> tString )
        <|> ( Lit.Boolean <$> tBool )
    
-- -----------------------------------------------------------------------------
-- Function Rules
    function <- rule $
        F.Function <$> opInfo0 <*> (varName <|> parens opName) <*> many binding <*> typesig0 <*> functionBody
    
    opInfo0 <- rule $
        opInfo <|> pure F.defOpInfo
    
    opInfo <- rule $
        parens (F.OpInfo <$> tInteger <*> (F.assocFromName <$> varId))
        
    functionBody <- rule $
        rsvp ":=" *> stmtblock
   
-- -----------------------------------------------------------------------------
-- Variables Rules   
    var <- rule $
      V.Variable <$> binding <*> typesig0 <*> (rsvp "^=" *> expr)


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
    
    
    return modl