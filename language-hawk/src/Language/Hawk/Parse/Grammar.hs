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
            <*> (rsvp ":" *> term)

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
            <*> (rsvp ":" *> term)


-- -----------------------------------------------------------------------------
-- Definition Declaration Rules

    def <- rule $
      let ex (n, _) c = Def n c
      in ex <$> varId <*> clause

    clause <- rule $
      Clause <$> many ((,) Explicit <$> pat) <*> (rsvp "=" *> term)


-- -----------------------------------------------------------------------------
-- Literal Rules

    val <- rule $
            ( first VInt <$> intLit )
        <|> ( first VFloat <$> floatLit )
        <|> ( first VChar <$> charLit )
        <|> ( first VBool <$> boolLit )

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
    term <- rule $
          termApp 
      <|> termArr
      <|> termLoli
      <|> aterm


    aterm <- rule $
          termVal
      <|> termVar
      <|> termOp
      <|> termCon
      <|> termPrim
      <|> termLam
      <|> termPi
      -- <|> termLPi
      <|> termLet
      <|> termCase
      <|> termDup
      <|> termParen
      <|> termWild


    termVal <- rule $
      let ex (v, l) = TLoc l $ TVal v
      in ex <$> val

    termVar <- rule $
      let ex (v, l) = TLoc l $ TVar v
      in ex <$> varId

    termOp <- rule $
      let ex (n, l) = TLoc l $ TVar n
      in ex <$> opId

    termCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    termPrim <- rule $
      let ex (txt, l1) a b = TLoc (l1 <> locOf b) $ TPrim (readPrim txt) a b
      in ex <$> primText <*> aterm <*> aterm

    termApp <- rule $
      let ex f x = let l = locOf f <> locOf x
                   in  TLoc l $ TApp f Explicit x
      in ex <$> term <*> aterm

    termLam <- rule $
      let ex (_,l1) arg ret@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLam Explicit arg ret
      in ex <$> rsvp "\\" <*> pat <*> (rsvp "->" *> term)

    termPi <- rule $
      let ex (_,l1) arg ret@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi Explicit arg ret
      in ex <$> rsvp "forall" <*> pat <*> (rsvp "." *> term)

    {-
    
    termLPi <- rule $
      let ex (_,l1) arg ret@(TLoc l2 _) =
            TLoc (l1<>l2) $ TPi Explicit arg ret
      in ex <$> rsvp "forall" <*> pat <*> (rsvp "." *> term)

    termArr <- rule

    -}

    termArr <- rule $
      let ex arg@(TLoc l1 _) ret@(TLoc l2 _)
            = TLoc (l1<>l2) $ explicitPi arg ret
      in (ex <$> term <*> (rsvp "->" *> aterm))
         <|> aterm

    termLoli <- rule $
      let ex arg@(TLoc l1 _) ret@(TLoc l2 _)
            = TLoc (l1<>l2) $ explicitLPi arg ret
      in (ex <$> term <*> (rsvp "-o" *> aterm))
         <|> aterm

    termLet <- rule $
      let ex (_,l1) ds t = TLoc (l1<>locOf t) $ TLet (NE.fromList ds) t
      in ex <$> rsvp "let" <*> block def <*> (rsvp "in" *> term)

    termCase <- rule $
      let ex (_, l1) p brs = TLoc (l1<>locOf brs) $ TCase p brs
      in ex <$> rsvp "case" <*> term <*> (rsvp "of" *> branches)

    termDup <- rule $
      let ex (_,l1) (n, l2) = TLoc (l1 <> l2) $ TDup n
      in ex <$> rsvp "dup" <*> varId

    termParen <- rule $
      let ex (t, l) =
            TLoc l $ TParen t
      in ex <$> parensLoc term

    termWild <- rule $
      let ex (_, l) = TLoc l $ TWild
      in ex <$> rsvp "_"


-- -----------------------------------------------------------------------------
-- Pattern Rules
    
    pat <- rule dpat

    dpat <- rule $
          parens patAnno
      <|> cpat

    cpat <- rule $
          parens patView
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
      let ex (n, l) = PLoc l $ PVar (N n) n
      in ex <$> varId

    patWild <- rule $
      let ex (_, l) = PLoc l PWild
      in ex <$> rsvp "_"

    patVal <- rule $
      let ex (v, l) = PLoc l $ PVal v
      in ex <$> val

    patCon <- rule $
      let ex (n, l) ps = PLoc (l <> locOf ps) $ PCon n ps
      in ex <$> conId <*> many ((,) Explicit <$> pat)

    patAnno <- rule $
      let ex p t = PLoc (locOf p <> locOf t) $ PAnno p t
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
      TopLevelDataDef <$> (dataDef' <|> dataDefAdt)


    dataDefAdt <- rule $
      let
        ex (n, l) cs = DataDef n [] cs
      in
        ex <$> (rsvp "type" *> conId)
           <*> (rsvp "where" *> block constr)

    adtConstr <- rule $
      let ex (n, _) ts = ConstrDef n $ foldr explicitPi TWild ts
      in ex <$> conId <* rsvp ":" <*> many term

    dataDef' <- rule $
      let
        ex (n, l) cs = DataDef n [] cs
      in
        ex <$> (rsvp "type" *> conId)
           <*> (rsvp "=" *> constrs)

    constrs <- rule $
      sep (rsvp "|") constr

    constr <- rule $
      let ex (n, _) ts = ConstrDef n $ foldr explicitPi TWild ts
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

    return topLevelDef