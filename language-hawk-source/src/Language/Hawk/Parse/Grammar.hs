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
toplevel :: Grammar r (Prod r String Token Decl)
toplevel = mdo

-- -----------------------------------------------------------------------------
-- Declaration Rules

    decl <- rule $ (linefold decl') <|> (decl' <* eof)

    decl' <- rule $
        ( expDecl
      <|> defDecl
      <|> sigDecl)
      <?> "Declaration"

    expDecl <- rule $
      (ExpDecl <$> cexp) <?> "Top-level Expression"

    defDecl <- rule $
      (DefDecl <$> def) <?> "Function"

    sigDecl <- rule $
      (SigDecl <$> sig) <?> "Signature"


-- -----------------------------------------------------------------------------
-- Declaration Rules


    def <- rule $
      let ex (n, _) xs body = Def n (fst <$> xs) body
      in ex <$> varId <*> many varId <*> (rsvp "=" *> exp)

    sig <- rule $
      let ex (n, _) t = Sig n t
      in ex <$> varId
            <*> (rsvp ":" *> typ)


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
      expType <|> cexp

    cexp <- rule $
      expLam <|> bexp

    bexp <- rule $
      expApp <|> aexp

    aexp <- rule $
          (expVar <?> "variable")
      <|> (expVal <?> "value")
      <|> (expPrim <?> "primitive operation")


    -- Terms
    expVar <- rule $
      let ex (v, l) = ELoc l $ EVar v
      in ex <$> varId

    expVal <- rule $
      let ex (v, l) = ELoc l $ EVal v
      in ex <$> val

    expPrim <- rule $
      let ex (i, l) = ELoc l $ EPrim (readPrimInstr i)
      in ex <$> primId

    -- Evaluation
    expApp <- rule $
      let ex f@(ELoc l1 _) x@(ELoc l2 _)
            = ELoc (l1<>l2) $ EApp f x
      in ex <$> bexp <*> aexp

    expLam <- rule $
      let ex (_,l1) (arg, _) ret@(ELoc l2 _) =
            ELoc (l1<>l2) $ ELam arg ret
      in ex <$> rsvp "\\" <*> varId <*> (rsvp "->" *> exp)

    -- Annotations
    expType <- rule $
      let ex e t =
            ELoc (locOf e<>locOf t) $ EType e t
      in ex <$> cexp <*> (rsvp ":" *> typ)


-- -----------------------------------------------------------------------------
-- Type Rules

    typ <- rule $
      tyArr <|> atyp 

    atyp <- rule $
      tyCon

    tyCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    tyArr <- rule $
      let ex1 arg@(TLoc l1 _) ret@(TLoc l2 _)
            = TLoc (l1<>l2) $ TArr arg ret
      in ex1 <$> atyp <*> (rsvp "->" *> typ)


-- -----------------------------------------------------------------------------
-- Kind Rules


    kind <- rule $ kStar

    -- Kind Terms
    kStar <- rule $
      let ex (_, l) = KLoc l $ KStar
      in ex <$> rsvp "*"

{-
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
-}

    return decl