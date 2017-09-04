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
import Language.Hawk.Parse.Token (Token)
import Language.Hawk.Syntax
import Text.Earley
import Text.Earley.Mixfix

-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Token Token Decl)
toplevel = mdo
        
-- -----------------------------------------------------------------------------
-- Declaration Rules
    
    decl <- rule $
          forgn
      <|> fixity
      <|> sig
      <|> def
      -- <|> dataDef
      -- <|> classDef
      -- <|> instDef

-- -----------------------------------------------------------------------------
-- Foreign Rules

    forgn <- rule $
      Foreign <$> forgn'

    forgn' <- rule $
      rsvp "foreign" *>
        (forgnImport <|> forgnExport)

    forgnImport <- rule $
      let extract ft (srcN, _) (hkN, _) ty
            = ForeignImport ft (pack srcN) hkN ty
      in extract <$> (rsvp "import" *> forgnType) <*> strLit <*> (varId <|> conId <|> opId) <*>  typ

    forgnExport <- rule $
      let extract ft (n, _)
            = ForeignExport ft n
      in extract <$> (rsvp "export" *> forgnType) <*> (varId <|> conId <|> opId)

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


-- -----------------------------------------------------------------------------
-- Type Signature Declaration Rules

    sig <- rule $
      let ex (n, _) ty = Sig n ty
      in ex <$> (varId <|> parens (fst <$> opId))
            <*> (rsvp ":" *> typ)


-- -----------------------------------------------------------------------------
-- Definition Declaration Rules

    def <- rule $
      let ex (n, _) e = Def n e
      in ex <$> varId <*> exp


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
      let ex1 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TArr a b
          ex2 a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TLoli a b
      in
           (ex1 <$> btyp <*> (rsvp "->" *> typ))
       <|> (ex2 <$> btyp <*> (rsvp "-o" *> typ))
       <|> btyp
   

    btyp <- rule $
      let ex a@(TLoc l1 _) b@(TLoc l2 _) =
            TLoc (l1<>l2) $ TApp a b

      in   (ex <$> atyp <*> btyp)
       <|> atyp

    atyp <- rule $
      gtyCon
      <|> tyVar
      <|> (fst <$> parens typ)
    
    gtyCon <- rule $
      tyCon

    tyCon <- rule $
      let ex (n, l) = TLoc l $ TCon n
      in ex <$> conId

    tyVar <- rule $
      let ex (n, l) = TLoc l $ TVar n
      in ex <$> varId
      

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
      cexp

    cexp <- rule $
          expType
      <|> bexp

    bexp <- rule $
      expApp
      <|> expPrim
      <|> aexp

    aexp <- rule $
          (fst <$> parens cexp)
      <|> expVar
      <|> expLit

    expFree <- rule $
      let ex (_,l1) v e@(ELoc l2 _)  = ELoc (l1<>l2) $ EFree v e
      in ex <$> rsvp "free" <*> fmap fst varId <*> (rsvp "in" *> exp)


    -- Expression forms
    expType <- rule $
      let
        ex e@(ELoc l1 _) ty@(TLoc l2 _)
          = ELoc (l1<>l2) $ EType ty e
      in
        ex <$> exp <*> (rsvp ":" *> typ)

    expApp <- rule $
      let ex a@(ELoc l1 _) b@(ELoc l2 _)
            = ELoc (l1<>l2) $ EApp a b
      in ex <$> aexp <*> aexp

    expVar <- rule $
      let ex (v, l) = ELoc l $ EVar v
      in ex <$> varId

    expLit <- rule $
      let ex (lit, l) = ELoc l $ ELit lit
      in ex <$> lit
    
    expPrim <- rule $
      let ex (txt, l) = ELoc l $ EPrim (readPrim txt)
      in ex <$> primText


    return decl

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
      DataType
        <$> (rsvp "data" *> conName)
        <*> many varName
        <*> (rsvp ":" *> block tySig')


-}
