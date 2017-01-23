{-# LANGUAGE RecursiveDo #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Parse.Helpers
import Text.Earley
import Text.Earley.Mixfix



import qualified Language.Hawk.Compile.Package as Pkg
import qualified Language.Hawk.Parse.Lexer as L
import qualified Language.Hawk.Syntax.Alias as A
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.Function as F
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.MetaItem as MI
import qualified Language.Hawk.Syntax.MetaModule as MM
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.ModuleName as MN
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.Type as Ty
import qualified Language.Hawk.Syntax.Variable as V


-- -----------------------------------------------------------------------------
-- Grammar for Hawk
grammar :: Pkg.Name -> Grammar r (Prod r L.Token L.Token M.Source)
grammar pkgName = mdo

-- -----------------------------------------------------------------------------
-- Module Rules
    modl <- rule $ M.Module "" <$> items <* eof

    metamodl <- rule $ MM.MetaModule <$> metaItems <* eof
      

-- -----------------------------------------------------------------------------
-- Module Name Rules
    modName <- rule $ MN.Name pkgName <$> modName'
    
    modName' <- rule ((:) <$> modNameHead <*> many modNameRest)
    modNameHead <- rule modId
    modNameRest <- rule (op "." *> modNameHead)

-- -----------------------------------------------------------------------------
-- Item Rules
    items <- rule $
      linefolds item

    item <- rule $
          impItem
      <|> recordItem
      <|> fnItem
      <|> varItem
      <|> aliasItem
    
    
    impItem <- rule $
      I.impItem <$> (op "->" *> modName')
      
    aliasItem <- rule $
      I.aliasItem <$> typAlias
      
    recordItem <- rule $
      I.recItem <$> record
    
    fnItem <- rule $
      I.fnItem <$> function
       
    varItem <- rule $
      I.varItem <$> var


-- -----------------------------------------------------------------------------
-- Meta Item Rules
    metaItems <- rule $ many metaItem
    
    metaItem <- rule $ 
          impMItem
      <|> aliasMItem
      <|> recMItem
      <|> fnMItem
      <|> varMItem
    
    impMItem <- rule $
      MI.Import <$> (op "->" *> modName')
       
    aliasMItem <- rule $
      MI.Alias <$> typAlias
    
    recMItem <- rule $
      MI.Record <$> record
    
    fnMItem <- rule $ 
      MI.Function <$> (functionInfo <* rsvp ":=" <* stmtblock)
    
    varMItem <- rule $
      MI.Variable <$> (varInfo <* rsvp "^=" <* expr)

-- -----------------------------------------------------------------------------
-- Name rules
    varName <- rule $ varId
    
    conName <- rule $ conId
    
    name <- rule $ varName <|> conName
    
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
    
    typ <- mixfixExpressionSeparate typeOpsTable typApp
    
    typApp <- rule $ 
      Ty.apply <$> typVal <*> many typVal
    
    typVal <- rule $ tyTuple <|> typCon
      
    tyTuple <- rule $ Ty.tuple <$> (parens $ sep (op ",") typApp)
        
    typCon <- rule $ Ty.Con <$> conName

-- -----------------------------------------------------------------------------
-- Type Alias Rules
    typAlias <- rule $
      A.Alias <$> (conName <* op "=") <*> typ

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
        F.Function <$> (functionInfo <* rsvp ":=") <*> stmtblock
        
    functionInfo <- rule $
        F.FunctionInfo <$> varName <*> many binding <*> typesig0
   
-- -----------------------------------------------------------------------------
-- Variables Rules   
    var <- rule $
      V.Variable <$> (varInfo <* rsvp "^=") <*> expr
    
    varInfo <- rule $
      V.VariableInfo <$> binding <*> typesig0


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
      Stmt.Return <$> (op "return" *> expr)
    

-- -----------------------------------------------------------------------------
-- Expression Rules
    expr <- rule $
          exprTyped
      <|> expr0
    
    
    exprTyped <- rule $
      E.Cast <$> expr0 <*> typesig
    
    
    expr0 <- rule $
          fexpr
      <|> aexpr
    
    
    fexpr <- rule $
        (\ (call:args) -> E.App call args) <$> some aexpr
    
    
    aexpr <- rule $
          litExpr
      <|> varExpr
      <|> conExpr
      <|> nestedExpr
    
    varExpr <- rule $ E.Var <$> varName
    conExpr <- rule $ E.Var <$> conName
    litExpr <- rule $ E.Lit <$> lit
    nestedExpr <- rule $ parens expr
    
    return modl
    

