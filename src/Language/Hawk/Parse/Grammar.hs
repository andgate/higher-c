{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Text (pack)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Prim
import Text.Earley
import Text.Earley.Mixfix

-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token ItemPs)
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
      Foreign <$> forgnType <*> (pack <$> tString) <*> tySig'

    forgnType <- rule $
      rsvp "ccall" *> pure ForeignC

-- -----------------------------------------------------------------------------
-- Expose Rules

    expose <- rule $
      Expose <$> (rsvp "expose" *> varName)

-- -----------------------------------------------------------------------------
-- Literal Rules

    lit <- rule $
            ( IntLit () <$> tInteger )
        <|> ( DblLit () <$> tReal )
        <|> ( ChrLit () <$> tChar )
        <|> ( StrLit () <$> tString )
        <|> ( BoolLit () <$> tBool )

-- -----------------------------------------------------------------------------
-- Type Rules
    
    typ <- rule $
      (TyFun () <$> btyp <*> (rsvp "->" *> typ))
      <|> btyp

    btyp <- rule $
      TyApp () <$> atyp <*> many atyp

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
      TyCon () <$> conName

    tyUnitCon <- rule $ 
      mkUnitTyCon <$> rsvp "(" <*> rsvp ")"

    tyFunCon <- rule $ 
      mkFunTyCon <$> rsvp "(" <*> rsvp "->" <*> rsvp ")"

    tyListCon <- rule $
       mkListTyCon <$> rsvp "[" <*> rsvp "]"

    tyTupleCon <- rule $
       mkTupleTyCon <$> rsvp "(" <*> some (rsvp ",") <*> rsvp ")"

    tyVar <- rule $
      TyVar () <$> varName
      
    tyTuple <- rule $ 
      TyTuple () <$> parens (sep (rsvp ",") typ)

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

    -- The expression chain, starting at aexp as the base with the highest precedence.
    exp <- rule $
          expDo
      <|> dexp

    dexp <- rule $
          expTypeHint
      <|> cexp

    cexp <- mixfixExpressionSeparate exprOpTable bexp

    bexp <- rule $
      expApp
      <|> expPrim
      <|> aexp

    aexp <- rule $
          parens dexp
      <|> expVar
      <|> expLit
      <|> expBottom


    -- Expression forms
    expTypeHint <- rule $
      ETypeHint () <$> exp <*> (rsvp "?" *> qtyp0)

    expDo <- rule $ 
      rsvp "do" *> expDo'

    expDo' <- rule $
      EDo () <$> (rsvp ":" *> stmtblk)

    expPrim <- rule $
      EPrim () <$> primInstr <*> aexp <*> aexp

    expApp <- rule $
      EApp () <$> aexp <*> some aexp

    expVar <- rule $
      EVar () <$> varName

    expLit <- rule $
      ELit () <$> lit
    
    expBottom <- rule $
        rsvp "_" *> pure (EBottom ())


-- -----------------------------------------------------------------------------
-- Primitive Instruction Rules

    primInstr <- rule $
          (prim "#add" *> pure PrimAdd)
      <|> (prim "#fadd" *> pure PrimFAdd)
      <|> (prim "#sub" *> pure PrimSub)
      <|> (prim "#fsub" *> pure PrimFSub)
      <|> (prim "#mul" *> pure PrimMul)
      <|> (prim "#fmul" *> pure PrimFMul)
      <|> (prim "#div" *> pure PrimDiv)
      <|> (prim "#udiv" *> pure PrimUDiv)
      <|> (prim "#sdiv" *> pure PrimSDiv)
      <|> (prim "#fdiv" *> pure PrimFDiv)

-- -----------------------------------------------------------------------------
-- Statement Rules   
    stmtblk <- rule $ block stmt
     
    stmt <- rule $
          (StmtExpr <$> exp)
      <|> (StmtExpr <$> stmtIf)
      <|> stmtReturn
      <|> (StmtDecl <$> nestedItem)

    stmtReturn <- rule $
      StmtExpr . EReturn () <$> (rsvp "return" *> exp)
    
    stmtIf <- rule $
      EIf () <$> (rsvp "if" *> dexp) <*> expDo' <*> stmtIfTail

    stmtElif <- rule $
      EIf () <$> (rsvp "elif" *> dexp) <*> expDo' <*> stmtIfTail

    stmtElse <- rule $
      rsvp "else" *> expDo'

    stmtIfTail <- rule $ optional $
      (ln' *> ln) *> (stmtElif <|> stmtElse)


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
      Val <$> varName <*> body

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