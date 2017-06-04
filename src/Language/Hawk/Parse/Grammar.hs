{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Monoid
import Data.Tree
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token (Token, tokenToText)
import Language.Hawk.Syntax.Generic (tyUnitConName, tyFunConName, tyListConName, tyTupleConName, emptyCtx)
import Language.Hawk.Syntax.Source
import Text.Earley



surroundList :: a -> [a] -> a -> [a]
surroundList a xs z = (a:xs) ++ [z]

-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token Item)
toplevel = mdo
        
-- -----------------------------------------------------------------------------
-- Item Rules
    item <- rule $ linefold $
          depDeclItem
      <|> exprDefItem
      <|> aliasDefItem
      <|> dataDefItem
      <|> typeClassDefItem

      
    depItem <- rule $
      DepItem <$> depDecl

    sigItem <- rule $
      TypeSigItem <$> typeSig

    funItem <- rule $
      FunItem <$> fun

    varItem <- rule $
      VarItem <$> var

    typeAliasItem <- rule $
      TypeAliasItem <$> typeAlias  
      
    newTypeItem <- rule $
      NewTypeItem <$> newType

    dataTypeItem <- rule $
      DataTypeItem <$> dataType
      
    typeClassItem <- rule $
      TypeClass <$> typeClass

    instItem <- rule $
      instItem <$> typeClassInst


-- -----------------------------------------------------------------------------
-- Dependency Rules   
    depDecl <- rule $
        Dep <$> depQual <*> depPath <*> depAlias
      
    depQual <- rule $
          (rsvp "->"  *> pure False)
      <|> (rsvp "=>" *> pure True)
      
    depAlias <- rule $
          (rsvp "@" *> fmap Just conIdText)
      <|> pure Nothing


-- -----------------------------------------------------------------------------
-- Dependency Path Rules

    depPath <- rule $
        depMod <|> depTargetCon

    depMod <- rule $
        DepModule <$> conIdText
                  <*> depNext

    depNext <- rule $
        rsvp "." *> (depTerm <|> depTargets)
    
    depTerm <- rule $
        depTarget <|> depMod
    
    depTarget <- rule $
        depTargetItem <|> depTargetCon

    depTargetItem <- rule $
        DepTarget <$> itemIdText
   
    depTargetCon <- rule $
        DepTarget <$> conIdText
    
    depTargets <- rule $
        parens (DepTargets <$> depHide0 <*> some depTerm)
    
    depHide0 <- rule $ 
        depHide <|> pure False
        
    depHide <- rule $
        rsvp "\\" *> pure True


-- -----------------------------------------------------------------------------
-- Literal Rules

    lit <- rule $
            ( IntNum <$> tInteger )
        <|> ( FloatNum <$> tReal )
        <|> ( Chr <$> tChar )
        <|> ( Str <$> tString )
        <|> ( Boolean <$> tBool )


-- -----------------------------------------------------------------------------
-- Name rules

    varName       <- rule $ name varId
    conName       <- rule $ name conId
    opName        <- rule $ name opId
    mixfixName    <- rule $ name mixfixId
    mixfixBlkName <- rule $ name (mixfixId <|> mixfixblkId)
        
    itemName <- rule $ 
        varName <|> parens opName <|> mixfixBlkName
       
    itemId <- rule $ 
        varId <|> mixfixId <|> mixfixblkId 
        
    itemIdText <- rule $ 
        tokenToText <$> itemId
       
    conIdText <- rule $
        tokenToText <$> conId

-- -----------------------------------------------------------------------------
-- Type Rules
    
    typ <- rule $
      (TypeFun <$> btyp <*> (rsvp "->" *> typ))
      <|> btyp

    btyp <- rule $
      TypeApp <$> atyp <*> many atyp

    atyp <- rule $
      gtyCon
      <|> tyVar
      <|> tyTuple
      <|> tyList
      <|> parens typ
    
    gtyCon <- rule $
      tyCon
      <|> tyUnitCon
      <|> tyFunCon
      <|> tyListCon
      <|> tyTupleCon

    tyCon <- rule $
      TypeCon <$> conName

    tyUnitCon <- rule $ 
      TypeCon tyUnitConName <* (rspv "(" *> rsvp ")")

    tyFunCon <- rule $ 
       TypeCon tyFunConName <* parens (rsvp "->")

    tyListCon <- rule $
       TypeCon tyListConName <* (rspv "[" *> rsvp "]")

    tyTupleCon <- rule $
       TypeCon . tyTupleConName . length <$> (rspv "(" *> rsvp ")")

    tyVar <- rule $
      TypeVar <$> varName
      
    tyTuple <- rule $ 
      TypeTuple <$> parens (sep (rsvp ",") typ)

    tyList <- rule $
      brackets typ


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
-- Variable Rules

    var <- rule $
        Var <$> (rsvp "var" *> varName) <*> varDeclBody0

    varDeclBody0 <- rule $
      optional varDeclBody
    
    varDeclBody <- rule $
          (rsvp ":" *> stmtBlock)
      <|> (rsvp "=" *> expr)


-- -----------------------------------------------------------------------------
-- Type Context Rules

    typeCtx0 <- rule $
        typeCtx <|> pure emptyCtx
       
    typeCtx <- rule $
        (Context <$> typeCtx') <* rsvp "=>"
    
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
        
    typeCtxTVar <- rule varId


-- -----------------------------------------------------------------------------
-- Type Signature Rules
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
        <|> rsvp "->"
        <|> rsvp "=>"
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
    
    
    return item