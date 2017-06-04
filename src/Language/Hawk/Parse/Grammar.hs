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
-- Type Context Rules

    qtyp0 <- rule $
        qtyp <|> typ
       
    qtyp <- rule $
        QType <$> (Context <$> tyCtx) <*> (rsvp "=>" *> tpy)
    
    tyCtx <- rule $
        Context <$> tyAsserts

    tyAsserts <- rule $
      parens (sep' (rsvp ",") tyAssert)
      <|> tyAssert
    
    tyAssert <- rule $
        TyAssert <$> conId <*> many typ


-- -----------------------------------------------------------------------------
-- Expression Rules

    expr <- rule $
          exprTyped
      <|> expr0
    
    
    exprTyped <- rule $
      ExprTypeAnnot <$> expr0 <*> (rsvp "?" *> typ)
    
    
    expr0 <- rule $
          fexpr
      <|> aexpr
    
    fexpr <- rule $
        ExprApp <$> bexpr <*> many aexpr
    
    aexpr <- rule $
          litExpr
      <|> bexpr
      
    bexpr <- rule $ 
          varExpr
      <|> conExpr
      <|> nestedExpr
    
    varExpr <- rule $ ExprVar <$> varName
    conExpr <- rule $ ExprCon <$> conName
    litExpr <- rule $ ExprLit <$> lit

    nestedExpr <- rule $ parens expr


-- -----------------------------------------------------------------------------
-- Statement Rules   
    stmtblk <- rule $ block stmt
    
    stmt <- rule $
          (StmtExpr <$> expr)
      <|> stmtDecl
      <|> stmtIf
      <|> stmtWhile
      <|> stmtReturn
      
    stmtDecl <- rule $
      (StmtVar <$> var)
      <|> (StmtFun <$> fun)
      <|> (StmtSig <$> tySig)
    
    stmtRet <- rule $
      StmtReturn <$> (rsvp "return" *> expr)    


-- -----------------------------------------------------------------------------
-- Body Rules

    body <- rule $
            bodyBlock
        <|> bodyExpr

    bodyBlock <- rule $
      BodyBlock <$> (rsvp ":" *> stmtblk)

    bodyExpr <- rule $
      BodyExpr <$> (rsvp "=" *> expr)


-- -----------------------------------------------------------------------------
-- Variable Rules

    var <- rule $
      Var <$> (rsvp "var" *> varName) <*> optional body


-- -----------------------------------------------------------------------------
-- Variable Rules

    fun <- rule $
      Fun <$> (rsvp "fun" *> varName) <*> many varName <*> body


-- -----------------------------------------------------------------------------
-- Type Signature Rules

    tySig <- rule $
      TypeSig <$> (rsvp "sig" *> varName) <*> (rsvp "?" *> qtyp0)


-- -----------------------------------------------------------------------------
-- New Type Rules

    newType <- rule $
      NewType <$> (rsvp "newtype" *> conName) <*> many varName <*> (rsvp "=" *> typ)

-- -----------------------------------------------------------------------------
-- Type Alias Rules

    typeAlias <- rule $
      TypeAlias <$> (rsvp "type" *> conName) <*> many varName <*> (rsvp "=" *> typ)


-- -----------------------------------------------------------------------------
-- Type Class Rules

    typeClass <- rule $
      TypeClass <$> context0 (rsvp "class" *> conName) <*> many varName <*> (rsvp "=" *> typ)

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
    
    
    return item