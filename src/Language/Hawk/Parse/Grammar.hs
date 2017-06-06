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
import Language.Hawk.Syntax.Generic
import Text.Earley


import qualified Language.Hawk.Syntax.Source as Src


surroundList :: a -> [a] -> a -> [a]
surroundList a xs z = (a:xs) ++ [z]

-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token Src.Item)
toplevel = mdo
        
-- -----------------------------------------------------------------------------
-- Item Rules
    item <- rule $ linefold $
          (DepItem <$> depDecl)
      <|> (SigItem <$> tySig)
      <|> (VarItem <$> var)
      <|> (FunItem <$> fun)
      <|> (NewTyItem <$> newType)
      <|> (TyAliasItem <$> typeAlias)
      <|> (TyClassItem <$> typeClass)
      <|> (TyInstItem <$> typeClassInst)
      <|> (DataItem <$> dataType)


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
      pure (TypeCon tyUnitConName) <* (rsvp "(" *> rsvp ")")

    tyFunCon <- rule $ 
      pure (TypeCon tyFunConName) <* parens (rsvp "->")

    tyListCon <- rule $
       pure (TypeCon tyListConName) <* (rsvp "[" *> rsvp "]")

    tyTupleCon <- rule $
       TypeCon . tyTupleConName . length <$> parens (some $ rsvp ",")

    tyVar <- rule $
      TypeVar <$> varName
      
    tyTuple <- rule $ 
      TypeTuple <$> parens (sep (rsvp ",") typ)

    tyList <- rule $
      brackets typ


-- -----------------------------------------------------------------------------
-- Type Context Rules

    qtyp0 <- rule $
        qtyp 
        <|> (QType (TyContext []) <$> typ)
       
    qtyp <- rule $
        QType <$> tyCtx' <*> typ
    
    tyCtx' <- rule $
        tyCtx <* rsvp "=>"

    tyCtx <- rule $
        TyContext <$> tyAsserts

    tyAsserts <- rule $
        parens (sep (rsvp ",") tyAssert)
        <|> mono tyAssert
    
    tyAssert <- rule $
        TyAssert <$> conName <*> many atyp


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

    stmtIf <- rule $
      StmtIf <$> (rsvp "if" *> expr)
             <*> (rsvp ":" *> stmtblk)
             <*> optional ((rsvp "else" *> rsvp ":") *> stmtblk)

    stmtWhile <- rule $
      StmtWhile <$> (rsvp "while" *> expr)
                <*> (rsvp ":" *> stmtblk) 
    
    stmtReturn <- rule $
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
      rsvp "var" *> var'

    var' <- rule $
      Var <$> varName <*> optional body


-- -----------------------------------------------------------------------------
-- Variable Rules

    fun <- rule $
      rsvp "fun" *> fun'

    fun' <- rule $
      Fun <$> varName <*> many varName <*> body


-- -----------------------------------------------------------------------------
-- Type Signature Rules

    tySig <- rule $
      rsvp "sig" *> tySig'

    tySig' <- rule $
      TypeSig <$> varName <*> (rsvp "?" *> qtyp0)


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
      TypeClass <$> (rsvp "class" *> optional tyCtx')
                <*> conName
                <*> many varName
                <*> (rsvp ":" *> typeClassBody)

    
    typeClassBody <- rule $
      block $
          (Left <$> fun')
      <|> (Right <$> tySig')


-- -----------------------------------------------------------------------------
-- Type Class Instance Rules

    typeClassInst <- rule $
      TypeClassInst
        <$> (rsvp "inst" *> optional tyCtx')
        <*> conName
        <*> some atyp
        <*> (rsvp ":" *> block fun')

-- -----------------------------------------------------------------------------
-- Data Type Rules

    dataType <- rule $
      DataType
        <$> (rsvp "data" *> conName)
        <*> some varName
        <*> (rsvp ":" *> block tySig')


    return item