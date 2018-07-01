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
        ( termDecl
      <|> funDecl
      <|> sigDecl)
      <?> "Declaration"

    termDecl <- rule $
      (TermDecl <$> term) <?> "Top-level Expression"

    funDecl <- rule $
      (FunDecl <$> fun) <?> "Function"

    sigDecl <- rule $
      (SigDecl <$> sig) <?> "Signature"


-- -----------------------------------------------------------------------------
-- Declaration Rules


    fun <- rule $
      let ex (n, _) xs body = Fun n (fst <$> xs) body
      in ex <$> varId <*> many varId <*> (rsvp "=" *> term)

    sig <- rule $
      let ex (n, _) t = Sig n t
      in ex <$> varId
            <*> (rsvp ":" *> term)


-- -----------------------------------------------------------------------------
-- Value Rules

    val <- rule $
            ( first VInt <$> intLit )
        <|> ( first VFloat <$> floatLit )
        <|> ( first VChar <$> charLit )
        <|> ( first VBool <$> boolLit )


-- -----------------------------------------------------------------------------
-- Term Rules

    -- The expression precedence chain, starting at aexp as the base with the highest precedence.
    term <- rule $
      termAnn <|> cterm

    cterm <- rule $
          termLam
      <|> termPi
      <|> termSigma
      <|> bterm

    bterm <- rule $
      termApp <|> aterm

    aterm <- rule $
          (termTy <?> "Type")
      <|> (termLn <?> "Linear")
      <|> (termVar <?> "variable")
      <|> (termCon <?> "constructor")
      <|> (termVal <?> "value")
      <|> (termPrim <?> "operation")
      <|> (termParen <?> "parens")


    -- Terms
    termTy <- rule $
      let ex (_, l) = TLoc l Type
      in ex <$> rsvp "Type"

    termLn <- rule $
      let ex (_, l) = TLoc l Linear
      in ex <$> rsvp "Linear"

    termVar <- rule $
      let ex (v, l) = TLoc l $ TVar v
      in ex <$> varId

    termCon <- rule $
      let ex (c, l) = TLoc l $ TCon c
      in ex <$> conId

    termVal <- rule $
      let ex (v, l) = TLoc l $ TVal v
      in ex <$> val

    termPrim <- rule $
      let ex (i, l) t1 t2 = TLoc l $ TPrim (readPrimInstr i) t1 t2
      in ex <$> primId <*> aterm <*> aterm

    -- Evaluation
    termApp <- rule $
      let ex f@(TLoc l1 _) x@(TLoc l2 _)
            = TLoc (l1<>l2) $ TApp f x
      in ex <$> bterm <*> aterm

    termLam <- rule $
      let ex (_,l1) (n, _) mt body@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLam n mt body
      in ex <$> rsvp "\\" <*> varId <*> termLamType0 <*> (rsvp "." *> term)

    termLamType0 <- rule $
      termLamType <|> pure Nothing

    termLamType <- rule $
      Just <$> (rsvp ":" *> term)

    termPi <- rule $
      let ex (v, l1) t1 t2@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi v t2 t1
      in ex <$> (varId <* rsvp "@") <*> bterm <*> (rsvp "->" *> term)

    termSigma <- rule $
      let ex t1@(TLoc l1 _) t2@(TLoc l2 _) =
            TLoc (l1<>l2) $ TSigma t1 t2
      in ex <$> bterm <*> (rsvp "->" *> term)

    -- Annotations
    termAnn <- rule $
      let ex t t' =
            TLoc (locOf t<>locOf t') $ TAnn t t'
      in ex <$> cterm <*> (rsvp ":" *> term)

    termParen <- rule $
      let ex (t, l) =
            TLoc l $ TParen t
      in ex <$> parensLoc term

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