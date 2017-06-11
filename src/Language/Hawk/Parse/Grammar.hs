{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Prim
import Text.Earley
import Text.Earley.Mixfix

-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token SrcItem)
toplevel = mdo
        
-- -----------------------------------------------------------------------------
-- Item Rules
    item <- rule $ linefold $
          (DepItem <$> depDecl)
      <|> (ForeignItem <$> forgn)
      <|> (ExposeItem <$> expose)
      <|> (SigItem <$> tySig)
      <|> (VowItem <$> vow)
      <|> (VarItem <$> var)
      <|> (ValItem <$> val)
      <|> (FunItem <$> fun)
      <|> (NewTyItem <$> newType)
      <|> (TyAliasItem <$> typeAlias)
      <|> (TyClassItem <$> typeClass)
      <|> (TyInstItem <$> typeClassInst)
      <|> (DataItem <$> dataType)

    

    nestedItem <- rule $
      (NestedVar <$> var)
      <|> (NestedVal <$> val)
      <|> (NestedFun <$> fun)
      <|> (NestedVow <$> vow)
      <|> (NestedSig <$> tySig)


-- -----------------------------------------------------------------------------
-- Name rules

    modNameText <- rule $
        _nameText <$> modName

    conNameText' <- rule $
        _nameText <$> conName'

    varNameText <- rule $ 
        _nameText <$> varName

    opNameText <- rule $
        _nameText <$> opName

-- -----------------------------------------------------------------------------
-- Dependency Rules
    depDecl <- rule $
        Dep <$> depQual <*> depPath <*> depAlias

    depQual <- rule $
          (rsvp "->"  *> pure False)
      <|> (rsvp "=>" *> pure True)

    depAlias <- rule $
          (rsvp "@" *> fmap Just modNameText)
      <|> pure Nothing


-- -----------------------------------------------------------------------------
-- Dependency Path Rules

    depPath <- rule $
        depPath'
        <|> (DepBase <$> modNameText)


    depPath' <- rule $
        DepPath <$> modNameText
                <*> depPathNext
    
    depPathNext <- rule $
        rsvp "." *> (depPath' <|> depBase <|> depSpecify)


    depBase <- rule $
        DepBase <$> (varNameText <|> modNameText <|> conNameText')
    

    depSpecify <- rule $
        parens (DepSpecify <$> depHide0 <*> some depSpecifiers)

    depSpecifiers <- rule $
        depPath' <|> depBase
    
    depHide0 <- rule $ 
        depHide <|> pure False
        
    depHide <- rule $
        rsvp "\\" *> pure True

-- -----------------------------------------------------------------------------
-- Foreign Rules

    forgn <- rule $
      rsvp "foreign" *> forgn'

    forgn' <- rule $
      Foreign <$> forgnType <*> tySig'

    forgnType <- rule $
      rsvp "ccall" *> pure ForeignC

-- -----------------------------------------------------------------------------
-- Expose Rules

    expose <- rule $
      Expose <$> (rsvp "expose" *> varName)

-- -----------------------------------------------------------------------------
-- Literal Rules

    lit <- rule $
            ( IntNum <$> tInteger )
        <|> ( FloatNum <$> tReal )
        <|> ( Chr <$> tChar )
        <|> ( Str <$> tString )
        <|> ( Boolean <$> tBool )

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
      sqrBrackets typ


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

    -- Going to need to introduce qnames into the lexer for
    -- this to work out properly. Will also require a qname splitter
    exp <- mixfixExpressionSeparate exprOpTable bexp


    bexp <- rule $
      (EApp <$> aexp <*> some aexp)
      <|> aexp

    aexp <- rule $
          parens exp
      <|> (EVar <$> varName)
      <|> (ELit <$> lit)
     -- <|> (ETyped <$> expAtom <*> (rsvp "?" *> typ))
      <|> expBottom

    expVar <- rule $
      EVar <$> varName
    
    expBottom <- rule $
        rsvp "_" *> pure EBottom



-- -----------------------------------------------------------------------------
-- Statement Rules   
    stmtblk <- rule $ block stmt
    
    stmt <- rule $
          (StmtExpr <$> exp)
      <|> (StmtDecl <$> nestedItem)


-- -----------------------------------------------------------------------------
-- Body Rules

    body <- rule $
            bodyBlock
        <|> bodyExpr

    bodyBlock <- rule $
      BodyBlock <$> (rsvp ":" *> stmtblk)

    bodyExpr <- rule $
      BodyExpr <$> (rsvp "=" *> exp)


-- -----------------------------------------------------------------------------
-- Variable Rules

    var <- rule $
      rsvp "var" *> var'

    var' <- rule $
      Var <$> varName <*> optional body


-- -----------------------------------------------------------------------------
-- Value Rules

    val <- rule $
      rsvp "val" *> val'

    val' <- rule $
      Val <$> varName <*> optional body

-- -----------------------------------------------------------------------------
-- Function Rules

    fun <- rule $
      rsvp "fun" *> fun'

    fun' <- rule $
      Fun <$> varName <*> many varName <*> body

-- -----------------------------------------------------------------------------
-- Vow Rules

    vow <- rule $
      rsvp "vow" *> vow'

    vow' <- rule $
      Vow <$> varName <*> many vowType

    vowType <- rule $
      (rsvp "var" *> pure VowVar)
      <|> (rsvp "val" *> pure VowVal)
      <|> (rsvp "ref" *> pure VowRef)

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
        <*> many varName
        <*> (rsvp ":" *> block tySig')


    return item