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
toplevel :: Grammar r (Prod r String Token TopLevelDef)
toplevel = mdo


-- -----------------------------------------------------------------------------
-- Declaration Rules
    
    topLevelDef <- rule $ linefold $
          (topLevelFnDef
      <|> topLevelSig
      <|> topLevelAliasDef
      <|> topLevelDataDef
      <|> topLevelClassDef
      <|> topLevelInstDef
      <|> topLevelForeignDef
      <|> topLevelFixityDef)
      <?> "Top-Level Definition"


    topLevelFnDef <- rule $
      (TopLevelFnDef <$> def) <?> "Function"

    topLevelSig <- rule $
      (TopLevelSig <$> sig) <?> "Signature"

    topLevelAliasDef <- rule $
      (TopLevelAliasDef <$> aliasDef) <?> "Type Alias"

    topLevelDataDef <- rule $
      (TopLevelDataDef <$> dataDef) <?> "DataType"

    topLevelClassDef <- rule $
      (TopLevelClassDef <$> classDef) <?> "Class"

    topLevelInstDef <- rule $
      (TopLevelInstDef <$> instDef) <?> "Class Instance"

    topLevelForeignDef <- rule $
      (TopLevelForeignDef <$> forgnDef) <?> "Foreign Function"
    
    topLevelFixityDef <- rule $
      (TopLevelFixityDef <$> fixityDecl) <?> "Fixity Declaration"




-- -----------------------------------------------------------------------------
-- Type Signature Declaration Rules

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
      in ex <$> conSId

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

    qtyp <- rule $
        QType <$> tyContext <*> typ

    tyContext <- rule $
          (tyAsserts <* rsvp "=>")
      <|> pure [] 

    tyAsserts <- rule $
        parens (sep (rsvp ",") tyAssert)
        <|> mono tyAssert
    
    tyAssert <- rule $
        IsIn <$> (fst <$> conSId) <*> many atyp

-- -----------------------------------------------------------------------------
-- Statement Rules


-- -----------------------------------------------------------------------------
-- Expression Rules

    -- The expression precedence chain, starting at aexp as the base with the highest precedence.
    exp <- rule $
      cexp

    eexp <- rule $
      expType <|> dexp

    dexp <- rule $
      expSeq <|> cexp

    -- new cexp for control flow
    cexp <- rule $
      expCase <|> expIf
              <|> expDo
              <|> expLam
              <|> bexp
      
    bexp <- rule $
      expApp <|> aexp

    aexp <- rule $
          (expVal <?> "value")
      <|> (expVar <?> "variable")
      <|> (expOp <?> "operator")
      <|> (expCon <?> "constructor")
      <|> (expPrim <?> "primitive expression")
      <|> (expParen <?> "parenthetical expression")
      <|> (expWild <?> "wildcard expression")


    -- Terms
    expVar <- rule $
      let ex (v, l) = ELoc l $ EVar v
      in ex <$> varId

    expVal <- rule $
      let ex (v, l) = ELoc l $ EVal v
      in ex <$> val

    expOp <- rule $
      let ex (n, l) = ELoc l $ EOp n
      in ex <$> opId


    -- Magic
    expConS <- rule $
      let ex (n, l) = ELoc l $ EConS n
      in ex <$> conSId

    expConH <- rule $
      let ex (n, l) = ELoc l $ EConH n
      in ex <$> conHId

    expPrim <- rule $
      let ex (txt, l1) a b@(ELoc l2 _) 
            = ELoc (l1<>l2) $ EPrim (readPrim txt) a b
      in ex <$> primText <*> aexp <*> aexp


    -- Evaluation
    expApp <- rule $
      let ex f@(ELoc l1 _) x@(ELoc l2 _)
            = ELoc (l1<>l2) $ EApp f x
      in ex <$> bexp <*> aexp

    expLam <- rule $
      let ex (_,l1) arg ret@(ELoc l2 _) =
            ELoc (l1<>l2) $ ELam arg ret
      in ex <$> rsvp "\\" <*> pat <*> (rsvp "->" *> exp)


    -- Imperative, impure object names
    expSeq <- rule $
      let ex a@(ELoc l1 _) b@(ELoc l2 _) =
            ELoc (l1<>l2) $ ESeq a b
      in ex <$> exp <*> (rsvp ";" *> exp)

    expBind <- rule $
      let ex (v, l1) e@(ELoc l2 _) =
            ELoc (l1<>l2) $ EBind v e
      in ex <$> varId <*> (rsvp "<-" *> exp)

    expSet <- rule $
      let ex (v, l1) e@(ELoc l2 _) =
            ELoc (l1<>l2) $ ESet v e
      in ex <$> varId <*> (rsvp "=" *> exp)

    expFree <- rule $
      let ex (_,l1) (v, l2) =
            ELoc (l1<>l2) $ EFree v
      in ex <$> rsvp "free" <*> varId


    -- Control Flow
    expCase <- rule $
      let ex (_, l1) (v, _) e@(ELoc l2 _) =
            ELoc (l1<>l2) $ ECase v brs
      in ex <$> rsvp "case" <*> exp <*> (rsvp "of" *> branches)

    expIf <- rule $
      let ex (_, l1) p a b@(ELoc l2 _) =
            ELoc (l1<>l2) $ EIf p a b
      in ex <$> rsvp "if" <*> exp <*> (rsvp "then" *> exp) <*> (rsvp "else" *> exp)

    expDo <- rule $
      let ex (_, l1) es (_, l2) =
            ELoc (l1<>l2) $ EDo es
      in ex <$> rsvp "do" <*> (blk *> linefolds0 exp) <*> blk'


    -- Annotations
    expType <- rule $
      let ex e@(ELoc _ l1) t@(TLoc _ l2) =
            ELoc (l1<>l2) $ EType e t
      in ex <$> dexp <*> (rsvp ":" <*> typ)

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
      in ex <$> (pat <* rsvp "->") <*> bexp


-- -----------------------------------------------------------------------------
-- Type Alias Rules

    aliasDef <- rule $
      AliasDef <$> (fst <$> conId)
               <*> many (fst <$> varId)
               <*> (rsvp "=" *> typ)

-- -----------------------------------------------------------------------------
-- Data Type Rules

    dataDef <- rule $
      DataDef <$> (fst <$> conId)
              <*> many (fst <$> varId)
              <*> (rsvp ":=" *> constrs)

    constrs <- rule $
      sep (rsvp "|") (constr <|> recConstr)

    constr <- rule $
      ConstrDef <$> (fst <$> conId)
                <*> many atyp

    recConstr <- rule $
      RecordDef <$> (fst <$> conId) <*> recFields

    recFields <- rule $ curlys $ commaSep $
      (,) <$> (fst <$> varId) <*> (rsvp ":" *> typ)

-- -----------------------------------------------------------------------------
-- Type Class Rules

    classDef <- rule $
      ClassDef <$> (rsvp "class" *> tyContext)
               <*> (fst <$> conId)
               <*> many atyp
               <*> (rsvp "where" *> block0 sig)


-- -----------------------------------------------------------------------------
-- Type Class Instance Rules

    instDef <- rule $
      InstDef
        <$> tyContext
        <*> (fst <$> conId)
        <*> many atyp
        <*> (rsvp "where" *> block0 def)


-- -----------------------------------------------------------------------------
-- Foreign Rules

    forgnDef <- rule $
      rsvp "foreign" *> (forgnImport <|> forgnExport)

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

    fixityDecl <- rule $
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


    return topLevelDef