{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , TupleSections
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative hiding (optional)
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

    term0 <- rule $ optional term

    cterm <- rule $
          termLam
      <|> termPi
      <|> bterm

    cterm0 <- rule $ optional cterm

    bterm <- rule $
      termApp <|> aterm

    aterm <- rule $
          (termTy <?> "Type")
      <|> (termLn <?> "Linear")
      <|> (termVar <?> "variable")
      <|> (termCon <?> "constructor")
      <|> (termVal <?> "value")
      <|> (termPrim)
      <|> (termWild <?> "_")
      <|> (termSigma <?> "parens")


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
      let ex f@(TLoc l1 _) xs
            = TLoc (mconcat $ l1:(locOf <$> xs)) $ TApp f (NE.fromList xs)
      in ex <$> aterm <*> some aterm


    termLam <- rule $
      let ex (_,l1) ps body@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLam (NE.fromList ps) body
      in ex <$> rsvp "\\" <*> some pat <*> (rsvp "." *> term)

    termPi <- rule $
      termPiA <|> termPiB <|> termPiC

    termPiA <- rule $
      let ex t1@(TLoc l1 _) t2@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi ((PlicitPat l1 Explicit $ PLoc l1 $ PAnn PWild t2):| []) t1
      in ex <$> bterm <*> (rsvp "->" *> term)

    termPiB <- rule $
      let ex (_, l1) ps body@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi (NE.fromList ps) body
      in ex <$> rsvp "\\" <*> some plicitPat <*> (rsvp "->" *> term)

    termPiC <- rule $
      let ex ip@(PlicitPat l1 _ _) body@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi (ip :| []) body
      in ex <$> implicitPat <*> term

    termLet <- rule $
      let ex (_, l1) bs body@(TLoc l2 _) = TLoc (l1<>l2) $ TLet (NE.fromList bs) body
      in ex <$> rsvp "let" <*> some patBind <*> (rsvp "in" *> term)


    termSigma <- rule $
      let ex (_, l1) ps t (_, l2) = TLoc (l1<>l2) $ TSigma ps t
      in ex <$> rsvp "(" <*> many (pat <* rsvp ",") <*> term <*> rsvp ")"

    -- Annotations
    termAnn <- rule $
      let ex t t' =
            TLoc (locOf t<>locOf t') $ TAnn t t'
      in ex <$> cterm <*> (rsvp ":" *> term)

    termWild <- rule $
      let ex (_, l) = TLoc l $ TWild
      in ex <$> rsvp "_"

-- -----------------------------------------------------------------------------
-- Pattern Rules
    
    pat <- rule apat

    apat <- rule $
          patVar
      <|> patWild
      <|> patParens


    patVar <- rule $
      let ex (n, l) = PLoc l $ PVar n
      in ex <$> varId

    patWild <- rule $
      let ex (_, l) = PLoc l PWild
      in ex <$> rsvp "_"

    patAnn <- rule $
      let ex p@(PLoc l1 _) t@(TLoc l2 _)
            = PLoc (l1<>l2) $ PAnn p t
      in ex <$> (pat <* rsvp ":") <*> bterm
  
    patParens <- rule $
      let ex (p, l) = PLoc l $ PParen p
      in ex <$> parensLoc (patAnn <|> pat)    


    plicitPat <- rule $
      let ex (Just (v, l1)) p@(PLoc l2 _) = PlicitPat (l1<>l2) Implicit p 
          ex _ p@(PLoc l _) = PlicitPat l Explicit p
      in ex <$> optional (rsvp "@") <*> pat

    implicitPat <- rule $
      let ex (v, l1) p@(PLoc l2 _) = PlicitPat (l1<>l2) Implicit p 
      in ex <$> rsvp "@" <*> pat


    patBind <- rule $
      let ex p t = PatBind p t 
      in ex <$> pat <*> (rsvp "=" *> term)

{-
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