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
      <|> typeAlias
      <|> typeDef
      <|> dataS
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
            <*> (rsvp ":" *> typ)

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
      let ex (n, _) ty = fromSig $ Sig n ty
      in ex <$> (varId <|> parens (fst <$> opId))
            <*> (rsvp ":" *> typ)


-- -----------------------------------------------------------------------------
-- Definition Declaration Rules

    fn <- rule $
      let ex n xs e = fromFn $ Fn n xs e
      in ex <$> fnName <*> many name <*> (rsvp "=" *> exp)


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

    typ <- rule $
      ctyp
    
    ctyp <- rule $
      let ex1 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TArr a b
          ex2 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLoli a b
      in
           (ex1 <$> ctyp <*> (rsvp "->" *> btyp))
       <|> (ex2 <$> ctyp <*> (rsvp "-o" *> btyp))
       <|> btyp
   

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

{-
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

    -- The expression chain, starting at aexp as the base with the highest precedence.
    exp <- rule $
      dexp

    dexp <- rule $
          expType
      <|> cexp

    cexp <- rule $
          expFree
      <|> expIf
      <|> expLet
      <|> bexp


    bexp <- rule $
      let ex f x = let l = locExp f <> locExp x
                   in  ELoc l $ EApp f x
      in     (ex <$> bexp <*> aexp)
         <|> aexp


    aexp <- rule $
          expParen
      <|> expVar
      <|> expDup
      <|> expOp
      <|> expPrim
      <|> expLit



    expLet <- rule $
      let ex (_, l1) bs _ e@(ELoc l2 _) = ELoc (l1 <> l2) $ foldr ELet e bs
      in ex <$> rsvp "let" <*> block expLetBind  <*> rsvp "in" <*> exp


    expLetBind <- rule $
      let ex n e = (n, e)
      in ex <$> name <*> (rsvp "=" *> exp)


    expIf <- rule $
      let ex (_, l1) p a b@(ELoc l2 _) = ELoc (l1<>l2) $ EIf p a b
      in ex <$> rsvp "if" <*> exp <*> (rsvp "then" *> exp) <*> (rsvp "else" *> exp)


    expFree <- rule $
      let
        ex (_, l1) xs e@(ELoc l2 _)
            = ELoc (l1<>l2) $ EFree xs e
      in
        ex <$> rsvp "free" <*> some name <*> (rsvp "in" *> exp)


    -- Expression forms
    expType <- rule $
      let
        ex e@(ELoc l1 _) ty@(TLoc l2 _)
          = ELoc (l1<>l2) $ EType ty e
      in
        ex <$> bexp <*> (rsvp ":" *> typ)


    expOp <- rule $
      let ex (n, l) = ELoc l $ EVar n
      in ex <$> opId



    expVar <- rule $
      let ex (v, l) = ELoc l $ EVar v
      in ex <$> varId

    expDup <- rule $
      let ex (_,l1) n = ELoc (l1 <> locName' n) $ EDup n
      in ex <$> rsvp "dup" <*> name

    expLit <- rule $
      let ex (lit, l) = ELoc l $ ELit lit
      in ex <$> lit
    
    expPrim <- rule $
      let ex (txt, l1) a b = ELoc (l1<>locExp b) $ EPrim (readPrim txt) a b
      in ex <$> primText <*> aexp <*> aexp

    expParen <- rule $
      let ex (e, l) = ELoc l $ EParen e
      in ex <$> parens exp



-- -----------------------------------------------------------------------------
-- New Type Rules

    typeDef <- rule $
      let ex nl tvsl t = fromTDef $ TypeDef (wrapL nl) (wrapL <$> tvsl) t
      in ex <$> (rsvp "type" *> conId) <*> many varId <*> (rsvp "=" *> typ)

-- -----------------------------------------------------------------------------
-- Type Alias Rules

    typeAlias <- rule $
      let ex nl tvsl t = fromTAlias $ TypeAlias (wrapL nl) (wrapL <$> tvsl) t
      in ex <$> (rsvp "alias" *> conId) <*> many varId <*> (rsvp "=" *> typ)


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

-- -----------------------------------------------------------------------------
-- Data Type Rules

    dataS <- rule $
      let
        ex nl tvsl cs = fromData $ DataS (wrapL nl) (wrapL <$> tvsl) cs
      in
        ex <$> (rsvp "data" *> conId)
           <*> many varId
           <*> (rsvp "=" *> dataCons)


    dataCons <- rule $
      sep (rsvp "|") dataCon


    dataCon <- rule $
      dataCon' <|> dataRec


    dataCon' <- rule $
      let ex nl ts = DataCon (mkLocName nl) ts
      in ex <$> conId <*> many atyp


    dataRec <- rule $
      let ex nl fs =  DataRec (mkLocName nl) fs
      in ex <$> conId <*> curlys (commaSep recField)                         

  
    recField <- rule $
      let ex nl t = RecField (mkLocName nl) t
      in ex <$> varId <*> (rsvp "::" *> typ)


    return result
