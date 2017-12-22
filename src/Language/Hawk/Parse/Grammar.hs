{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
  #-}
module Language.Hawk.Parse.Grammar where

import Control.Applicative
import Data.Bifunctor
import Data.Monoid
import Data.Text (pack)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Lex.Token (Token)
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Term.Source
import Text.Earley
import Text.Earley.Mixfix


-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token Image)
toplevel = mdo
        
-- -----------------------------------------------------------------------------
-- Declaration Rules
    
    result <- rule $ linefold $
          fn
      <|> sig
      <|> typeS
      <|> forgn
      <|> fixity


    name <- rule $
      let ex (n, l) = NLoc l $ Name n
      in ex <$> varId


    opName <- rule $
      let ex (n, l) = NLoc l $ Name n
      in ex <$> opId

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
      in ex <$> (varId <|> parens (fst <$> opId))
            <*> (rsvp ":" *> term)


-- -----------------------------------------------------------------------------
-- Definition Declaration Rules

    fn <- rule $
      let ex n xs e = fromFn $ Fn n xs e
      in ex <$> fnName <*> many pat <*> (rsvp "=" *> term)


    fnName <- rule $
      name <|> (fst <$> parens opName)

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
            TLoc (l1<>l2) $ TPi (Name "", a) b
          ex2 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLPi (Name "", a) b
      in
          (ex1 <$> eterm <*> (rsvp "->" *> dterm))
      <|> (ex2 <$> eterm <*> (rsvp "-o" *> dterm))
      <|> dterm

    dterm <- rule $
          termHint
      <|> cterm


    cterm <- rule $
          termFree
      <|> termIf
      <|> termLet
      <|> bterm


    bterm <- rule $
      let ex f x = let l = locTerm f <> locTerm x
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

    termHint <- rule $
      let
        ex e@(TLoc l1 _) ty@(TLoc l2 _)
          = TLoc (l1<>l2) $ THint ty e
      in
        ex <$> dterm <*> (rsvp ":" *> term)


    termLet <- rule $
      let ex (_, l1) bs _ t@(TLoc l2 _) = TLoc (l1 <> l2) $ foldr TLet t bs
      in ex <$> rsvp "let" <*> block termLetBind  <*> rsvp "in" <*> term


    termLetBind <- rule $
      let ex n t = (n, t)
      in ex <$> name <*> (rsvp "=" *> term)


    termIf <- rule $
      let ex (_, l1) p a b@(TLoc l2 _) = TLoc (l1<>l2) $ TIf p a b
      in ex <$> rsvp "if" <*> term <*> (rsvp "then" *> term) <*> (rsvp "else" *> term)


    termFree <- rule $
      let
        ex (_, l1) xs e@(TLoc l2 _)
            = TLoc (l1<>l2) $ TFree xs e
      in
        ex <$> rsvp "free" <*> some name <*> (rsvp "in" *> term)


    termOp <- rule $
      let ex (n, l) = TLoc l $ TVar n
      in ex <$> opId

    termVar <- rule $
      let ex (v, l) = TLoc l $ TVar v
      in ex <$> varId

    termCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    termDup <- rule $
      let ex (_,l1) n = TLoc (l1 <> locName' n) $ TDup (readName n)
      in ex <$> rsvp "dup" <*> name

    termLit <- rule $
      let ex (lit, l) = TLoc l $ TLit lit
      in ex <$> lit
    
    termPrim <- rule $
      let ex (txt, l1) a b = TLoc (l1<>locTerm b) $ TPrim (readPrim txt) a b
      in ex <$> primText <*> aterm <*> aterm
      
    termParen <- rule $
      let ex (t, l) = TLoc l $ TParen t
      in ex <$> parens term


-- -----------------------------------------------------------------------------
-- Structured Type Rules

    typeS <- rule $
      let
        ex nl tvsl cs = fromTStruct $ TypeS (wrapL nl) (wrapL <$> tvsl) cs
      in
        ex <$> (rsvp "type" *> conId)
           <*> many varId
           <*> (rsvp "=" *> typeCons)


    typeCons <- rule $
      sep (rsvp "|") typeCon


    typeCon <- rule $
      typeCon' <|> recCon


    typeCon' <- rule $
      let ex = TypeCon . mkLocName
      in ex <$> conId <*> many aterm


    recCon <- rule $
      let ex =  RecCon . mkLocName
      in ex <$> conId <*> curlys (commaSep recLabel)                         

  
    recLabel <- rule $
      let ex = RecLabel . mkLocName
      in ex <$> varId <*> (rsvp ":" *> term)

-- -----------------------------------------------------------------------------
-- Pattern Rules
    pat <- rule $
          patVar
      <|> patWild
      <|> patConMono
      <|> patAs
      <|> patParens

    patVar <- rule $
      let ex (n, l) = PLoc l $ PVar n
      in ex <$> varId

    patWild <- rule $
      let ex (_, l) = PLoc l PWild
      in ex <$> rsvp "_"

    patAs <- rule $
      let ex (n, l1) (p, l2) = PLoc (l1<>l2) $ PAs n p
      in ex <$> varId <*> (rsvp "@" *> parens patCon0)

    patConMono <- rule $
      let ex (n, l) = PLoc l $ PCon n []
      in ex <$> conId

    patCon0 <- rule $
      let ex (n, l) ps = PLoc l $ PCon n ps
      in ex <$> conId <*> many pat

    patCon1 <- rule $
      let ex (n, l) ps = PLoc l $ PCon n ps
      in ex <$> conId <*> some pat

    patParens <- rule $
      let ex (p, l) = PLoc l $ PParen p
      in ex <$> parens (patType <|> patCon1)

    patType <- rule $
      let ex p t = PLoc (locPat p <> locTerm t) $ PHint t p
      in ex <$> (patCon0 <|> pat) <*> (rsvp ":" *> term)


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