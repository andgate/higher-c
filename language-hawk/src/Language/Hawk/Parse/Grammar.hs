{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , TupleSections
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Text (Text, pack)

import Language.Hawk.Parse.Helpers
import Language.Hawk.Lex.Token (Token)
import Language.Hawk.Syntax.Source
import Language.Hawk.Syntax.Source.Helpers

import Text.Earley
import Text.Earley.Mixfix

import qualified Data.List.NonEmpty as NE


-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token TopLevelDef)
toplevel = mdo


-- -----------------------------------------------------------------------------
-- Declaration Rules
    
    topLevelDef <- rule $ linefold $
          (TopLevelDef <$> def)
      <|> sigDef
      <|> dataDef
      <|> forgnDef
      <|> fixityDef

-- -----------------------------------------------------------------------------
-- Foreign Rules

    forgnDef <- rule $
      TopLevelForeignDef <$> forgn

    forgn <- rule $
      rsvp "foreign" *> forgn'

    forgn' <- rule $
      forgnImport <|> forgnExport

    forgnImport <- rule $
      let ex ft (srcN, l1) (hkN, l2) ty
            = ForeignImport ft (L l1 $ pack srcN) (L l2 hkN) ty
      in ex <$> (rsvp "import" *> forgnType)
            <*> strLit
            <*> (varId <|> conId <|> opId)
            <*> (rsvp ":" *> typ)

    forgnExport <- rule $
      let ex ft (n, l)
            = ForeignExport ft (L l n)
      in ex <$> (rsvp "export" *> forgnType)
         <*> (varId <|> conId <|> opId)

    forgnType <- rule $
      ForeignC <$ rsvp "ccall"


-- -----------------------------------------------------------------------------
-- Fixity Rules

    fixityDef <- rule $
      TopLevelFixityDef <$> fixity

    fixity <- rule $
      let ex fx (p, l) ops =
            Fixity fx (L l $ fromIntegral p) (wrapL <$> ops)
      in ex <$> fixityKind <*> intLit <*> some opId

    fixityKind <- rule $
      infixL <|> infixR <|> infixN <|> prefix <|> postfix

    infixL <- rule $
      InfixL <$ rsvp "infixl"

    infixR <- rule $
      InfixR <$ rsvp "infixr" 

    infixN <- rule $
      InfixN <$ rsvp "infix" 

    prefix <- rule $
      Prefix <$ rsvp "prefix" 

    postfix <- rule $
      Postfix <$ rsvp "postfix" 


-- -----------------------------------------------------------------------------
-- Type Signature Declaration Rules

    sigDef <- rule $
      TopLevelSig <$> sig

    sig <- rule $
      let ex (n, _) t = Sig n t
      in ex <$> (varId <|> parensLoc (fst <$> opId))
            <*> (rsvp ":" *> qtyp)


-- -----------------------------------------------------------------------------
-- Definition Declaration Rules

    def <- rule $
      let ex (n, _) c = Def n c
      in ex <$> varId <*> clause

    clause <- rule $
      Clause <$> many pat <*> (rsvp "=" *> exp)


-- -----------------------------------------------------------------------------
-- Literal Rules

    val <- rule $
            ( first VInt <$> intLit )
        <|> ( first VFloat <$> floatLit )
        <|> ( first VChar <$> charLit )
        <|> ( first VBool <$> boolLit )

-- -----------------------------------------------------------------------------
-- Type Rules

    typ <- rule $
      ctyp 

    ctyp <- rule $ 
      let ex1 arg@(TLoc l1 _) ret@(TLoc l2 _)
            = TLoc (l1<>l2) $ TArr arg ret
          ex2 arg@(TLoc l1 _) ret@(TLoc l2 _)
            = TLoc (l1<>l2) $ TLoli arg ret
      in     (ex1 <$> ctyp <*> (rsvp "->" *> btyp))
         <|> (ex2 <$> ctyp <*> (rsvp "-o" *> btyp))
         <|> btyp

    btyp <- rule $
      let ex f@(TLoc l1 _) x@(TLoc l2 _)
            = TLoc (l1<>l1) $ TApp f x
      in   (ex <$> btyp <*> atyp)
       <|> atyp

    atyp <- rule $
          gtyCon
      <|> tyVar
      <|> tyParens
    
    gtyCon <- rule $
          tyCon
      <|> tyUnit


    tyUnit <- rule $
      let ex (_, l1) (_, l2) = TLoc (l1<>l2) $ TCon "()"
      in ex <$> rsvp "(" <*> rsvp ")"

    tyCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    tyVar <- rule $
      let ex (n, l) = TLoc l $ TVar n
      in ex <$> varId

    tyParens <- rule $
      let ex (t, l) = TLoc l $ TParen t
      in ex <$> parensLoc typ

    tyWild <- rule $
      let ex (_, l) = TLoc l $ TWild
      in ex <$> rsvp "_"
      

-- -----------------------------------------------------------------------------
-- Type Context Rules

    qtyp0 <- rule $
        qtyp 
        <|> (QType [] <$> typ)
       
    qtyp <- rule $
        QType <$> tyAsserts <*> (rsvp "=>" *> typ)

    tyAsserts <- rule $
        parens (sep (rsvp ",") tyAssert)
        <|> mono tyAssert
    
    tyAssert <- rule $
        IsIn <$> (fst <$> conId) <*> many atyp


-- -----------------------------------------------------------------------------
-- Expression Rules

    -- The expression precedence chain, starting at aexp as the base with the highest precedence.
    exp <- rule $
      aexp


    aexp <- rule $
          expVal
      <|> expVar
      <|> expOp
      <|> expCon
      <|> expPrim
      <|> expLet
      <|> expCase
      <|> expDup
      <|> expParen
      <|> expWild




    expVar <- rule $
      let ex (v, l) = ELoc l $ EVar v
      in ex <$> varId
    
    expDup <- rule $
      let ex (_,l1) (n, l2) = ELoc (l1 <> l2) $ EDup n
      in ex <$> rsvp "dup" <*> varId

    expVal <- rule $
      let ex (v, l) = ELoc l $ EVal v
      in ex <$> val

    expCon <- rule $
      let ex (n, l) = ELoc l $ ECon n
      in ex <$> conId

    expOp <- rule $
      let ex (n, l) = ELoc l $ EOp n
      in ex <$> opId

    expPrim <- rule $
      let ex (txt, l1) a b@(ELoc l2 _) 
            = ELoc (l1<>l2) $ EPrim (readPrim txt) a b
      in ex <$> primText <*> aexp <*> aexp

    expApp <- rule $
      let ex f@(ELoc l1 _) x@(ELoc l2 _)
            = ELoc (l1<>l2) $ EApp f x
      in ex <$> exp <*> aexp

    expLam <- rule $
      let ex (_,l1) arg ret@(ELoc l2 _) =
            ELoc (l1<>l2) $ ELam arg ret
      in ex <$> rsvp "\\" <*> pat <*> (rsvp "->" *> exp)
    

    expLet <- rule $
      let ex (_,l1) ds b@(ELoc l2 _)
            = ELoc (l1<>l2) $ ELet (NE.fromList ds) b
      in ex <$> rsvp "let" <*> block def <*> (rsvp "in" *> exp)


    expCase <- rule $
      let ex (_, l1) p brs =
            let l2 = mconcat [ l | (l,_,_) <- brs]
            in ELoc (l1<>l2) $ ECase p brs
      in ex <$> rsvp "case" <*> exp <*> (rsvp "of" *> branches)

    

    expParen <- rule $
      let ex (e, l) = ELoc l $ EParen e
      in ex <$> parensLoc exp

    expWild <- rule $
      let ex (_, l) = ELoc l $ EWild
      in ex <$> rsvp "_"


-- -----------------------------------------------------------------------------
-- Pattern Rules
    
    pat <- rule dpat

    dpat <- rule $
          parens patType
      <|> cpat

    cpat <- rule $
          parens patAs
      <|> bpat

    bpat <- rule $
          parens patCon
      <|> apat

    apat <- rule $
          patVar
      <|> patWild
      <|> patVal
      <|> parens pat

    patVar <- rule $
      let ex (n, l) = PLoc l $ PVar n
      in ex <$> varId
    
    patVal <- rule $
      let ex (v, l) = PLoc l $ PVal v
      in ex <$> val

    patCon <- rule $
      let ex (n, l1) ps =
            let l2 = mconcat [ l | PLoc l _ <- ps]
            in PLoc (l2 <> l1) $ PCon n ps
      in ex <$> conId <*> many pat

    patAs <- rule $
      let ex (x, l1) (p, l2) = PLoc (l1 <> l2) $ PAs x p
      in ex <$> (varId <* rsvp "@") <*> parensLoc pat
    

    patType <- rule $
      let ex p@(PLoc l1 _) t@(TLoc l2 _)
            = PLoc (l1<>l2) $ PType p t
      in ex <$> (pat <* rsvp ":") <*> typ

    patWild <- rule $
      let ex (_, l) = PLoc l PWild
      in ex <$> rsvp "_"

    

-- -----------------------------------------------------------------------------
-- Branch Rules

    branches <- rule $ block branch

    branch <- rule $
      let ex p@(PLoc l1 _) e@(ELoc l2 _) = (l1<>l2, p, e)
      in ex <$> (pat <* rsvp "->") <*> exp


 -- -----------------------------------------------------------------------------
-- Structured Type Rules

    dataDef <- rule $
      TopLevelDataDef <$> (dataDef' <|> dataDefAdt)


    dataDefAdt <- rule $
      let
        ex (n, l) cs = DataDef n [] cs
      in
        ex <$> (rsvp "type" *> conId)
           <*> (rsvp "where" *> block constr)

    adtConstr <- rule $
      let ex (n, _) ts = ConstrDef n $ foldr TLoli TWild ts
      in ex <$> conId <* rsvp ":" <*> many typ

    dataDef' <- rule $
      let
        ex (n, l) cs = DataDef n [] cs
      in
        ex <$> (rsvp "type" *> conId)
           <*> (rsvp "=" *> constrs)

    constrs <- rule $
      sep (rsvp "|") constr

    constr <- rule $
      let ex (n, _) ts = ConstrDef n $ foldr TLoli TWild ts
      in ex <$> conId <*> many typ


{-

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
-}

    return topLevelDef