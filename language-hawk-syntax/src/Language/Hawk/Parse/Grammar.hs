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
toplevel :: Grammar r (Prod r String Token SrcModule )
toplevel = mdo

-- -----------------------------------------------------------------------------
-- Module Rules

    srcMod <- rule $ undefined

    modDef <- rule $ undefined


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
-- Value Rules

    val <- rule $
            ( first VInt <$> intLit )
        <|> ( first VFloat <$> floatLit )
        <|> ( first VChar <$> charLit )
        <|> ( first VBool <$> boolLit )


-- -----------------------------------------------------------------------------
-- Expression Rules

    -- The expression precedence chain, starting at aexp as the base with the highest precedence.
    exp <- rule $
      expType <|> dexp

    dexp <- rule $
      expCase <|> expIf
              <|> expDo
              <|> expLam
              <|> cexp


    cexp <- rule $
      expLet <|> bexp


    bexp <- rule $
      expApp <|> aexp


    aexp <- rule $
          (expVar <?> "variable")
      <|> (expCon <?> "constructor")
      <|> (expOp <?> "operator")
      <|> (expVal <?> "value")
      <|> (expTuple <?> "operator")
      <|> (expArray <?> "operator")
      <|> (expParen <?> "parenthetical expression")
      <|> (expWild <?> "wildcard expression")


    -- Terms
    expVar <- rule $
      let ex (v, l) = ELoc l $ EVar v
      in ex <$> varId

    expOp <- rule $
      let ex (op, l) = ELoc l $ EOp op
      in ex <$> opId

    expVal <- rule $
      let ex (v, l) = ELoc l $ EVal v
      in ex <$> val

    expCon <- rule $
      let ex (n, l) = ELoc l $ ECon n
      in ex <$> conId


    -- Evaluation
    expApp <- rule $
      let ex f@(ELoc l1 _) x@(ELoc l2 _)
            = ELoc (l1<>l2) $ EApp f x
      in ex <$> bexp <*> aexp

    expLam <- rule $
      let ex (_,l1) arg ret@(ELoc l2 _) =
            ELoc (l1<>l2) $ ELam arg ret
      in ex <$> rsvp "\\" <*> some pat <*> (rsvp "->" *> exp)

    expLet <- rule $
      let ex (_, l1) stmts e@(ELoc l2 _) =
            ELoc (l1<>l2) $ ELet stmts e
      in ex <$> rsvp "let" <*> block letStmt <*> (rsvp "in" *> exp)


    -- Helpers for Let Rules
    letStmt <- rule $
      letBind <|> letDef

    letBind <- rule $
      let ex p e = LBind p e
      in ex <$> pat <*> (rsvp "=" *> exp)
    
    letDef <- rule $
      let ex (v, _) ps e = LDef v ps e
      in ex <$> varId <*> many pat <*> (rsvp "=" *> exp)


    -- Control Flow
    expRec <- rule $
      let ex e@(ELoc l1 _) (recs, l2) = ELoc (l1<>l2) $ ERec e recs
      in ex <$> exp <*> curlysLoc (commaSep ( (,) <$> (fst <$> varId) <*> (rsvp "=" *> exp) )) 

    expCase <- rule $
      let ex (_, l1) e brs (_, l2) =
            ELoc (l1<>l2) $ ECase e brs
      in ex <$> rsvp "case" <*> exp <*> (rsvp "of" *> (blk *> linefolds branch)) <*> blk'

    expIf <- rule $
      let ex (_, l1) p a b@(ELoc l2 _) =
            ELoc (l1<>l2) $ EIf p a b
      in ex <$> rsvp "if" <*> exp <*> (rsvp "then" *> exp) <*> (rsvp "else" *> exp)

    expDo <- rule $
      let ex (_, l1) es (_, l2) =
            ELoc (l1<>l2) $ EDo es
      in ex <$> rsvp "do" <*> (blk *> linefolds0 exp) <*> blk'


    -- Built-in Containers
    expTuple <- rule $
      let ex (ts, l) = ELoc l $ ETuple ts
      in ex <$> parensLoc (commaSep exp)

    expArray <- rule $
      let ex (t, l) = ELoc l $ EArray t
      in ex <$> bracesLoc (commaSep' exp)


    -- Annotations
    expType <- rule $
      let ex e t =
            ELoc (locOf e<>locOf t) $ EType e t
      in ex <$> dexp <*> (rsvp ":" *> typ)

    expParen <- rule $
      let ex (e, l) = ELoc l $ EParen e
      in ex <$> parensLoc exp

    expWild <- rule $
      let ex (_, l) = ELoc l $ EWild
      in ex <$> rsvp "_"


-- -----------------------------------------------------------------------------
-- Type Rules

    typ <- rule $
      tyKind <|> dtyp 

    dtyp <- rule $
      tyForall <|> ctyp

    ctyp <- rule $ 
      tyApp <|> btyp

    btyp <- rule $
      tyArr <|> atyp

    atyp <- rule $
          tyVar
      <|> gtyCon
      <|> tyOp
      <|> tyParens
    
    gtyCon <- rule $
          tyCon
      <|> tyTuple
      <|> tyArray


    -- Terms
    tyVar <- rule $
      let ex (n, l) = TLoc l $ TVar n
      in ex <$> varId

    tyCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    tyOp <- rule $
      let ex (op, l) = TLoc l $ TOp op
      in ex <$> opId


    -- Application
    tyApp <- rule $
      let ex f@(TLoc l1 _) x@(TLoc l2 _)
            = TLoc (l1<>l1) $ TApp f x
      in ex <$> btyp <*> atyp

    tyArr <- rule $
      let ex1 arg@(TLoc l1 _) ret@(TLoc l2 _)
            = TLoc (l1<>l2) $ TArr arg ret
      in ex1 <$> ctyp <*> (rsvp "->" *> btyp)

    tyForall <- rule $
      let ex1 (_,l1) xs t@(TLoc l2 _)
            = TLoc (l1<>l2) $ TForall (fst <$> xs) t
      in ex1 <$> rsvp "forall" <*> many varId <*> (rsvp "." *> dtyp)
    

    -- Record Type
    tyRow <- rule $
      tyRow1 <|> tyRow2

    tyRow1 <- rule $
      let ex (recs,l) = TLoc l $ TRow recs Nothing
      in ex <$> curlysLoc (commaSep ( (,) <$> (fst <$> varId) <*> (rsvp ":" *> atyp) ))

    tyRow2 <- rule $
      let ex (_,l1) recs (v, _) (_,l2) = TLoc (l1<>l2) $ TRow recs (Just v)
      in ex <$>  rsvp "{" <*> commaSep ((,) <$> (fst <$> varId) <*> (rsvp ":" *> atyp)) <*> (rsvp "|" *> varId) <*> rsvp "}"


    -- Simple containers
    tyTuple <- rule $
      let ex (ts, l) = TLoc l $ TTuple ts
      in ex <$> parensLoc (commaSep' atyp)

    tyArray <- rule $
      let ex (t, l) = TLoc l $ TArray t
      in ex <$> bracesLoc typ


    -- Annotations
    tyKind <- rule $
      let ex1 t@(TLoc l1 _ ) k@(KLoc l2 _)
            = TLoc (l1<>l2) $ TKind t k
      in ex1 <$> dtyp <*> (rsvp ":" *> kind)

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
        IsIn <$> (fst <$> conId) <*> many atyp



-- -----------------------------------------------------------------------------
-- Kind Rules


    kind <- rule $
      bkind

    bkind <- rule $
      kArr <|> akind

    akind <- rule $
      kStar <|> kRow <|> kParen <|> kWild


    -- Kind Terms
    kStar <- rule $
      let ex (_, l) = KLoc l $ KStar
      in ex <$> rsvp "*"

    -- Row Kinds!!
    kRow <- rule $
      let ex (_, l1) k@(KLoc l2 _) = KLoc (l1<>l2) $ KRow k
      in ex <$> rsvp "Row" <*> akind

    -- Basic function kind
    kArr <- rule $
      let ex a@(KLoc l1 _) b@(KLoc l2 _) =
              KLoc (l1<>l2) $ KArr a b
      in ex <$> akind <*> (rsvp "->" *> bkind)

    -- Misc
    kParen <- rule $
      let ex (k, l) = KLoc l $ KParen k
      in ex <$> parensLoc kind

    kWild <- rule $
      let ex (_, l) = KLoc l $ KWild
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
      <|> patRec

    patVar <- rule $
      let ex (n, l) = PLoc l $ PVar n
      in ex <$> varId
    
    patVal <- rule $
      let ex (v, l) = PLoc l $ PVal v
      in ex <$> val

    patAs <- rule $
      let ex (x, l1) (p, l2) = PLoc (l1 <> l2) $ PAs x p
      in ex <$> (varId <* rsvp "@") <*> parensLoc pat

    patCon <- rule $
      let ex (n, l1) ps =
            let l2 = mconcat [ l | PLoc l _ <- ps]
            in PLoc (l2 <> l1) $ PCon n ps
      in ex <$> conId <*> many pat

    patRec <- rule $
      let ex (recs, l) = PLoc l $ PRec recs
      in ex <$> parensLoc (commaSep ((,) <$> (fst <$> varId) <*> pat))
    

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
               <*> (rsvp "has" *> block0 sig)


-- -----------------------------------------------------------------------------
-- Type Class Instance Rules

    instDef <- rule $
      InstDef
        <$> tyContext
        <*> (fst <$> conId)
        <*> many atyp
        <*> (rsvp "has" *> block0 def)


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


    return srcMod