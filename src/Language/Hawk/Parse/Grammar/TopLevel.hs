{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar.TopLevel where

import Control.Applicative
import Data.Monoid
import Data.Tree
import Language.Hawk.Parse.Helpers
import Text.Earley


import qualified Language.Hawk.Parse.Lexer as Lex


import qualified Language.Hawk.Syntax.AliasDefinition as AD
import qualified Language.Hawk.Syntax.TypeClassDefinition as TCD
import qualified Language.Hawk.Syntax.DataDefinition as DD
import qualified Language.Hawk.Syntax.ExpressionDeclaration as EDec
import qualified Language.Hawk.Syntax.ExpressionDefinition as EDef
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDeclaration as TD



surroundList :: a -> [a] -> a -> [a]
surroundList a xs z = (a:xs) ++ [z]

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
    varName       <- rule $ name $ varId
    conName       <- rule $ name $ conId
    opName        <- rule $ name $ opId
    mixfixName    <- rule $ name $ mixfixId
    mixfixBlkName <- rule $ name $ mixfixId <|> mixfixblkId
        
    itemName <- rule $ 
        varName <|> parens opName <|> mixfixBlkName
       
    itemId <- rule $ 
       varId <|> mixfixId <|> mixfixblkId 
        
    itemIdText <- rule $ 
       Lex.tokenToText <$> itemId
       
    conIdText <- rule $
        Lex.tokenToText <$> conId

-- -----------------------------------------------------------------------------
-- Item Path Rules

    depPath <- rule $
        depPathTerm
        
    depPathTerm <- rule $
        depPathTarget <|> depPathModule
        
    depPathModule <- rule $
        I.DepModule <$> conIdText <*> depPathSub
        
    depPathSub <- rule $
            depPathTargetSub
        <|> depPathTargets
    
    depPathTargetSub <- rule $
        rsvp "." *> depPathTarget
    
    depPathTarget <- rule $
        I.DepTarget <$> itemIdText
    
    depPathTargets <- rule $
        parens (I.DepTargets <$> depPathTargetsIncl0 <*> many depPathTerm)
    
    depPathTargetsIncl0 <- rule $ 
      depPathTargetsIncl <|> pure True
        
    depPathTargetsIncl <- rule $
        rsvp "\\" *> pure False
        
-- -----------------------------------------------------------------------------
-- Item Rules
    items <- rule $
      linefolds item

    item <- rule $
          depDeclItem
      <|> exprDefItem
      <|> aliasDefItem
      <|> dataDefItem
      <|> typeClassDefItem
    
    
      
    depDeclItem <- rule $
      I.DepDecl <$> depDecl  
      
    exprDefItem <- rule $
      I.ExprDef <$> exprDef  

    aliasDefItem <- rule $
      I.AliasDef <$> aliasDef  
      
    dataDefItem <- rule $
      I.DataDef <$> dataDef
      
    typeClassDefItem <- rule $
      I.TypeClassDef <$> typeClassDef
      
      
    opInfo0 <- rule $
        opInfo <|> pure OI.defOpInfo

-- -----------------------------------------------------------------------------
-- Dependency Declaration Rules   
    depDecl <- rule $
        I.Dep <$> depProp <*> depQual <*> depPath <*> depAlias
      
    depProp <- rule $
          (rsvp "->"  *> pure False)
      <|> (rsvp "=>" *> pure True)

    depQual <- rule $
          (rsvp "!"  *> pure True)
      <|> (pure False)
      
    depAlias <- rule $
          (rsvp "@" *> fmap Just conIdText)
      <|> pure Nothing

-- -----------------------------------------------------------------------------
-- Operator Information Rules    
    opInfo <- rule $
        OI.OpInfo <$> tInteger <*> opAssoc
    
    opAssoc <- rule $
          (con "L" *> opAssocL)
      <|> (con "R" *> opAssocR)
      <|> (con "N" *> opAssocN)
      
    opAssocL <- rule $ pure OI.AssocL
    opAssocR <- rule $ pure OI.AssocR
    opAssocN <- rule $ pure OI.AssocN

-- -----------------------------------------------------------------------------
-- Expression Definition Rules
    exprDecl <- rule $
        EDec.ExprDecl <$> exprDefName <*> opInfo0 <*> exprVars <*> typesig0
    
    exprDef <- rule $
        EDef.ExprDef <$> exprDecl <*> exprDefBody

    exprDefName <- rule $
        varName <|> parens opName
    
    exprDefOp <- rule $
        rsvp "=" <|> rsvp ":=" <|> rsvp ":"
     
    exprVars <- rule $
        many exprVar
    
    exprVar <- rule $
        varName <|> name (rsvp "_")
    
    exprDefBody <- rule $
        exprDefOp *> raw



-- -----------------------------------------------------------------------------
-- Type Context Rules

    typeCtx0 <- rule $
        typeCtx <|> pure QT.emptyCtx
       
    typeCtx <- rule $
        (QT.Context <$> typeCtx') <* rsvp "=>"
    
    typeCtx' <- rule $
        typeCtxConstraintList <|> parens typeCtxConstraintList
    
    typeCtxConstraintList <- rule $
        sep' (rsvp ",") typeCtxConstraint
    
    typeCtxConstraint <- rule $
        prepend conId typeCtxArgList

        
    typeCtxArgList <- rule $
        concat <$> some typeCtxArg
    
    typeCtxArg <- rule $
        mono typeCtxTVar <|> typeCtxArgParens
        
    typeCtxArgParens <- rule $
        parens typeCtxArgList
        
    typeCtxTVar <- rule $
        varId

-- -----------------------------------------------------------------------------
-- Type Context Rules
    typesig0 <- rule $
        typesig <|> pure []
        
    typesig <- rule $
        rsvp "?" *> tipes
   
    tipes <- rule $
        concat <$> some tipe
   
    tipe <- rule $
        mono tipeId <|> tipeGroup
        
    tipeGroup <- rule $
        concat <$> parens (many tipe)
    
    -- A cheap hack to get reserved operators that can exist in the type
    -- namespace
    tipeId <- rule $ 
            varId
        <|> conId
        <|> opId
        <|> mixfixId
        <|> rsvp "=>"
        <|> rsvp ">"
        <|> rsvp "<"
        <|> rsvp "."
        <|> rsvp ","
        <|> rsvp "["
        <|> rsvp "]"
        <|> rsvp "_"
        <|> rsvp "\\"
        <|> rsvp "@"

-- -----------------------------------------------------------------------------
-- Type Declaration Rules

    typeDecl <- rule $
        TD.TypeDecl <$> typeCtx0 <*> typeDeclCons <*> opInfo0 <*> typeDeclArgs0
        
    typeDeclCons <- rule $
        conName <|> mixfixName
      
    typeDeclArgs0 <- rule $
         many typeDeclArg
        
    typeDeclArgs <- rule $
        some typeDeclArg
      
    typeDeclArg <- rule $
        mono typeDeclArgId <|> rawParens

    typeDeclArgId <- rule $
        varId <|> conId <|> mixfixId <|> parens opId
        
    
-- -----------------------------------------------------------------------------
-- Type Alias Rules

    aliasDef <- rule $
      AD.AliasDef <$> typeDecl <*> (rsvp "-" *> raw)
      

-- -----------------------------------------------------------------------------
-- Data Definition Rules

    dataDef <- rule $
      dataDefA <|> dataDefB
      
    dataDefA <- rule $
      DD.DataDef <$> typeDecl <*> dataDefABody
      
    dataDefB <- rule $
      DD.mkRecDef <$> typeDecl <*> dataDefBBody
    
    dataDefABody <- rule $
      rsvp ":-" *> block dataCons
      
    dataDefBBody <- rule $
      rsvp ":-" *> block dataRow
      
    
    dataCons <- rule $
      dataConsA <|> dataConsB
    
    dataConsA <- rule $
      DD.DataCons <$> conName <*> pure [] <*> dataConsBody
      
    dataConsB <- rule $
      DD.DataCons <$> conName <*> typesig <*> pure []
      
    dataConsType <- rule $
      rsvp "?" *> raw
      
      
    dataConsBody <- rule $
      dataConsBodyA <|> dataConsBodyB
      
    
    dataConsBodyA <- rule $
      rsvp ":" *> block dataRow
    
    dataRow <- rule $
      DD.DataRow <$> (Just <$> varName) <*> dataConsType
    
    
    dataConsBodyB <- rule $
      some dataConsArg
    
    dataConsArg <- rule $
       DD.DataRow Nothing <$> typeDeclArg
    
    
-- -----------------------------------------------------------------------------
-- Type Class Definition Rules

    typeClassDef <- rule $
      TCD.TypeClassDef <$> typeDecl <*> typeClassDefBody
    
    typeClassDefBody <- rule $
      rsvp ":~" *> block exprDef
      
      
-- -----------------------------------------------------------------------------
-- Raw Token Rules
      
    raw0 <- rule $
      raw <|> pure []
    
    raw <- rule $
      many notLayout <|> rawBlk <|> rawLn <|> rawParens
      
    rawBlk <- rule $
      surroundList <$> blk <*> raw0 <*> blk'
    
    rawLn <- rule $
      surroundList <$> ln <*> raw0 <*> ln'
      
    rawParens <- rule $
      surroundList <$> rsvp "(" <*> raw0 <*> rsvp ")"


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