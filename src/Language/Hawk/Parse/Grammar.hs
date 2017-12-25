{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
           , FlexibleContexts
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Bifunctor
import Data.Monoid
import Data.Text (Text, pack)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Lex.Token (Token)
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Term.Source
import Language.Hawk.Syntax.Pattern.Source
import Text.Earley
import Text.Earley.Mixfix


-- -----------------------------------------------------------------------------
-- Types Required by Grammar

type SourcePat = Pat (Term Text) Text
type SourceImage = Image Term Text SourcePat


-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token SourceImage)
toplevel = mdo


-- -----------------------------------------------------------------------------
-- Declaration Rules
    
    result <- rule $ linefold $
          fn
      <|> sig
      <|> dataDef
      <|> forgn
      <|> fixity

-- -----------------------------------------------------------------------------
-- Foreign Rules

    forgn <- rule $
      rsvp "foreign" *> forgn'

    forgn' <- rule $
      fromForeign <$>
        (forgnImport <|> forgnExport)

    forgnImport <- rule $
      let ex ft (srcN, l1) (hkN, l2) ty
            = ForeignImport ft (L l1 $ pack srcN) (L l2 hkN) ty
      in ex <$> (rsvp "import" *> forgnType)
            <*> strLit
            <*> (varId <|> conId <|> opId)
            <*> (rsvp ":" *> term)

    forgnExport <- rule $
      let ex ft (n, l)
            = ForeignExport ft (L l n)
      in ex <$> (rsvp "export" *> forgnType)
         <*> (varId <|> conId <|> opId)

    forgnType <- rule $
      rsvp "ccall" *> pure ForeignC


-- -----------------------------------------------------------------------------
-- Fixity Rules

    fixity <- rule $
      let ex fx (p, l) ops = fromFixity $
            Fixity fx (L l $ fromIntegral p) (wrapL <$> ops)
      in ex <$> fixityKind <*> intLit <*> some opId

    fixityKind <- rule $
      infixL <|> infixR <|> infixN <|> prefix <|> postfix

    infixL <- rule $
      rsvp "infixl" *> pure InfixL
      
    infixR <- rule $
      rsvp "infixr" *> pure InfixR
      
    infixN <- rule $
      rsvp "infix" *> pure InfixN

    prefix <- rule $
      rsvp "prefix" *> pure Prefix

    postfix <- rule $
      rsvp "postfix" *> pure Postfix


-- -----------------------------------------------------------------------------
-- Type Signature Declaration Rules

    sig <- rule $
      let ex (n, _) t = fromSig $ Sig n t
      in ex <$> (varId <|> parensLoc (fst <$> opId))
            <*> (rsvp ":" *> term)


-- -----------------------------------------------------------------------------
-- Definition Declaration Rules

    fn <- rule $
      let ex n xs e = fromFn $ Fn n xs e
      in ex <$> fnName <*> many pat <*> (rsvp "=" *> term)


    fnName <- rule $
      name <|> parens (fst <$> opId)

-- -----------------------------------------------------------------------------
-- Literal Rules

    lit <- rule $
            ( first IntLit <$> intLit )
        <|> ( first FloatLit <$> floatLit )
        <|> ( first CharLit <$> charLit )
        <|> ( first BoolLit <$> boolLit )

-- -----------------------------------------------------------------------------
-- Type Rules

{-
    typ <- rule $
      ctyp
    
    
   

    btyp <- rule $
      let ex f x = let l = (locType f <> locType x)
                   in TLoc l $ TApp f x
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
      let ex (e, l) = TLoc l $ TParen e
      in ex <$> parens typ
      

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

-}
-- -----------------------------------------------------------------------------
-- Expression Rules

    -- The expression precedence chain, starting at aexp as the base with the highest precedence.
    term <- rule dterm

    eterm <- rule $
      let ex1 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi ("", a) b
          ex2 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLPi (Name "", a) b
      in
          (ex1 <$> pat <*> (rsvp "->" *> term))
      <|> (ex2 <$> pat <*> (rsvp "-o" *> term))
      <|> dterm

    dterm <- rule $
          termAnnot
      <|> cterm


    cterm <- rule $
          termFree
      <|> termIf
      <|> termLet
      <|> bterm


    bterm <- rule $
      let ex f x = let l = locOf f <> locOf x
                   in  TLoc l $ TApp f x
      in     (ex <$> bterm <*> aterm)
         <|> aterm


    aterm <- rule $
          termParen
      <|> termVar
      <|> termCon
      <|> termDup
      <|> termOp
      <|> termPrim
      <|> termLit


    termVar <- rule $
      let ex (v, l) = TLoc l $ TVar v
      in ex <$> varId

    termOp <- rule $
      let ex (n, l) = TLoc l $ TVar n
      in ex <$> opId


    termLit <- rule $
      let ex (lit, l) = TLoc l $ TLit lit
      in ex <$> lit


    termCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    termPrim <- rule $
      let ex (txt, l1) a b = TLoc (l1 <> locOf b) $ TPrim (readPrim txt) a b
      in ex <$> primText <*> aterm <*> aterm



    termLet <- rule $
      let ex (_, l1) bs _ t@(TLoc l2 _) = TLoc (l1 <> l2) $ foldr TLet t bs
      in ex <$> rsvp "let" <*> block termLetBind  <*> rsvp "in" <*> term


    termLetBind <- rule $
      let ex n t = (n, t)
      in ex <$> name <*> (rsvp "=" *> term)


    termCase <- rule $
      let ex (_, l1) p brs = TLoc (l1<>locOf brs) $ TCase p brs
      in ex <$> rsvp "case" <*> term <*> (rsvp "of" *> branches)

      
    termDup <- rule $
      let ex (_,l1) (n, l2) = TLoc (l1 <> l2) $ TDup n
      in ex <$> rsvp "dup" <*> varId

    
    termFree <- rule $
      let
        ex (_, l1) xs e@(TLoc l2 _)
            = TLoc (l1<>l2) $ TFree (fst <$> xs) e
      in
        ex <$> rsvp "free" <*> some varId <*> (rsvp "in" *> term)


    termAnnot <- rule $
      let
        ex e@(TLoc l1 _) ty@(TLoc l2 _)
          = TLoc (l1<>l2) $ TAnnot e ty
      in
        ex <$> dterm <*> (rsvp ":" *> term)

    termSub <- rule $
      let
        ex (st, l) t
          = TLoc (l<>locOf t) $ TSub st t
      in
        ex <$> term <*> (rsvp "?" *> subtype)

    subtype <- rule $ 
      intuitive <|> linear

    intuitive <- rule $
      (TIn,) <$> rsvp "*"

    linear <- rule $
      (TLin,) <$> rsvp "o"
      
    termParen <- rule $
      parens term


-- -----------------------------------------------------------------------------
-- Pattern Rules
    
    pat <- rule dpat

    dpat <- rule $
          parens patAnnot
      <|> cpat

    cpat <- rule $
          parens patView
      <|> bpat

    bpat <- rule $
          parens patConPats
      <|> apat

    apat <- rule $
          patVar
      <|> patWild
      <|> patLit
      <|> patCon
      <|> parens pat

    patVar <- rule $
      let ex (n, l) = PLoc l $ PVar (N n) n
      in ex <$> varId

    patWild <- rule $
      let ex (_, l) = PLoc l PWild
      in ex <$> rsvp "_"

    patLit <- rule $
      let ex (lit, l) = PLoc l $ PLit lit
      in ex <$> lit

    patCon <- rule $
      let ex (n, l) = PLoc l $ PCon n []
      in ex <$> conId

    patConPats <- rule $
      let ex (n, l) ps = PLoc (l <> locOf ps) $ PCon n ps
      in ex <$> conId <*> many pat

    patAnnot <- rule $
      let ex p t = PLoc (locOf p <> locOf t) $ PAnnot p t
      in ex <$> (pat <* rsvp ":") <*> term

    patView <- rule $
      let ex t p = PLoc (locOf t <> locOf p) $ PView t p
      in ex <$> (term <* rsvp "@") <*> pat


-- -----------------------------------------------------------------------------
-- Branch Rules

    branches <- rule $ block branch

    branch <- rule $
      let ex p t = (p, t)
      in ex <$> (pat <* rsvp "->") <*> term


 -- -----------------------------------------------------------------------------
-- Structured Type Rules

    dataDef <- rule $
      let
        ex (n, l) cs = fromTStruct $ DataS n cs
      in
        ex <$> (rsvp "type" *> conId)
           <*> (rsvp "=" *> constrs)

    constrs <- rule $
      sep (rsvp "|") constr

    constr <- rule $
      let ex (n, _) ts = Constr n $ foldr (TPi . PAnnot PWild) TWild ts
      in ex <$> conId <*> many term   


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

    return result