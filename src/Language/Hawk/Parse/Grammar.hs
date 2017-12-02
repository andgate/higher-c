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
          sig
      <|> fn
      -- <|> dataDef
      -- <|> classDef
      -- <|> instDef


    name <- rule $
      let ex (n, l) = NLoc l $ Name n
      in ex <$> varId


    opName <- rule $
      let ex (n, l) = NLoc l $ Name n
      in ex <$> opId

-- -----------------------------------------------------------------------------
-- Foreign Rules
{-
    forgn <- rule $
      Foreign <$> forgn'

    forgn' <- rule $
      rsvp "foreign" *>
        (forgnImport <|> forgnExport)

    forgnImport <- rule $
      let ex ft (srcN, _) (hkN, _) ty
            = ForeignImport ft (pack srcN) hkN ty
      in ex <$> (rsvp "import" *> forgnType) <*> strLit <*> (varId <|> conId <|> opId) <*> (rsvp ":" *> typ)

    forgnExport <- rule $
      let ex ft (n, _)
            = ForeignExport ft n
      in ex <$> (rsvp "export" *> forgnType) <*> (varId <|> conId <|> opId)

    forgnType <- rule $
      rsvp "ccall" *> pure ForeignC


-- -----------------------------------------------------------------------------
-- Fixity Rules

    fixity <- rule $
      let ex fx (p, _) ops =
            Fixity fx (fromIntegral p) (map fst ops)
      in ex <$> fixity' <*> intLit <*> some opId

    fixity' <- rule $
      infixL <|> infixR <|> infixN

    infixL <- rule $
      rsvp "infixl" *> pure InfixL
      
    infixR <- rule $
      rsvp "infixr" *> pure InfixR
      
    infixN <- rule $
      rsvp "infix" *> pure InfixN
-}

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
      let ex (txt, l) = ELoc l $ EPrim (readPrim txt)
      in ex <$> primText

    expParen <- rule $
      let ex (e, l) = ELoc l $ EParen e
      in ex <$> parens exp


    return result

{-
-- -----------------------------------------------------------------------------
-- New Type Rules

    newType <- rule $
      NewType <$> (rsvp "newtype" *> conName) <*> many varName <*> (rsvp "=" *> typ)

-- -----------------------------------------------------------------------------
-- Type Alias Rules

    typeAlias <- rule $
      TypeAlias <$> (rsvp "type" *> conName) <*> many varName <*> (rsvp "=" *> typ)


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


-- -----------------------------------------------------------------------------
-- Data Type Rules

    dataType <- rule $
      DataDecl
        <$> (rsvp "data" *> conName)
        <*> many varName
        <*> (rsvp ":" *> block tySig')

    conDecl <- rule $
      
-}
