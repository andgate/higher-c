{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar.TopLevel where

import Control.Applicative
import Data.Monoid
import Data.Tree
import Language.Hawk.Parse.Helpers
import Text.Earley


import qualified Language.Hawk.Parse.Lexer as Lex


import qualified Language.Hawk.Syntax.AliasDefinition as AD
import qualified Language.Hawk.Syntax.TypeClassDefinition as TCD
import qualified Language.Hawk.Syntax.DataDefinition as DD
import qualified Language.Hawk.Syntax.ExpressionDeclaration as EDec
import qualified Language.Hawk.Syntax.ExpressionDefinition as EDef
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDeclaration as TD



surroundList :: a -> [a] -> a -> [a]
surroundList a xs z = (a:xs) ++ [z]

-- -----------------------------------------------------------------------------
-- Grammar for Hawk
toplevel :: Grammar r (Prod r Lex.Token Lex.Token M.Source)
toplevel = mdo

-- -----------------------------------------------------------------------------
-- Module Rules
    modl <- rule $ M.Module "" <$> items <* eof


-- -----------------------------------------------------------------------------
-- Literal Rules
    lit <- rule $
            ( Lit.IntNum <$> tInteger )
        <|> ( Lit.FloatNum <$> tReal )
        <|> ( Lit.Chr <$> tChar )
        <|> ( Lit.Str <$> tString )
        <|> ( Lit.Boolean <$> tBool )
    

-- -----------------------------------------------------------------------------
-- Name rules
    varName       <- rule $ name $ varId
    conName       <- rule $ name $ conId
    opName        <- rule $ name $ opId
    mixfixName    <- rule $ name $ mixfixId
    mixfixBlkName <- rule $ name $ mixfixId <|> mixfixblkId
        
    itemName <- rule $ 
        varName <|> parens opName <|> mixfixBlkName

-- -----------------------------------------------------------------------------
-- Module path rules
    paths <- rule $
        many path
    
    path <- rule $
            pathItem
        <|> pathCon
    
    pathItem <- rule $
        liftA2 Node itemName (pure [])
      
    pathCon <- rule $
        liftA2 Node conName pathExt
        
    pathExt <- rule $ 
            parens paths
        <|> mono (rsvp "." *> path)
        <|> pure []
        



-- -----------------------------------------------------------------------------
-- Item Rules
    items <- rule $
      linefolds item

    item <- rule $
          importItem
      <|> exportItem
      <|> exprDefItem
      <|> aliasDefItem
      <|> dataDefItem
      -- <|> typeClassDefItem
    
    
    importItem <- rule $
      I.Import <$> (rsvp "->" *> paths)
      
    exportItem <- rule $
      I.Export <$> (rsvp "<-" *> paths)
      
    exprDefItem <- rule $
      I.ExprDef <$> exprDef
      
    aliasDefItem <- rule $
      I.AliasDef <$> aliasDef  
      
    dataDefItem <- rule $
      I.DataDef <$> dataDef
      
    typeClassDefItem <- rule $
      I.TypeClassDef <$> typeClassDef


-- -----------------------------------------------------------------------------
-- Expression Definition Rules
    exprDecl <- rule $
        EDec.ExprDecl <$> exprDefName <*> opInfo0 <*> many exprVar <*> typesig0
    
    exprDef <- rule $
        EDef.mkExprDef <$> exprDecl <*> exprDefBody

    exprDefName <- rule $
        varName <|> parens opName
    
    exprDefOp <- rule $
        rsvp "=" <|> rsvp ":=" <|> rsvp ":"
     
    
    exprVar <- rule $
        varName <|> name (rsvp "_")
    
    exprDefBody <- rule $
        exprDefOp *> raw
    
        
    opInfo0 <- rule $
        opInfo <|> pure OI.defOpInfo
    
    opInfo <- rule $
        OI.OpInfo <$> tInteger <*> (OI.assocFromName <$> varName)



-- -----------------------------------------------------------------------------
-- Type Context Rules

    typeCtx0 <- rule $
        typeCtx <|> pure QT.emptyCtx
       
    typeCtx <- rule $
        (QT.Context <$> typeCtx') <* rsvp "=>"
    
    typeCtx' <- rule $
        typeCtxConstraintList <|> parens typeCtxConstraintList
    
    typeCtxConstraintList <- rule $
        sep' (rsvp ",") typeCtxConstraint
    
    typeCtxConstraint <- rule $
        prepend conId typeCtxArgList

        
    typeCtxArgList <- rule $
        concat <$> some typeCtxArg
    
    typeCtxArg <- rule $
        mono typeCtxTVar <|> typeCtxArgParens
        
    typeCtxArgParens <- rule $
        parens typeCtxArgList
        
    typeCtxTVar <- rule $
        varId

-- -----------------------------------------------------------------------------
-- Type Context Rules
    typesig0 <- rule $
        typesig <|> pure []
        
    typesig <- rule $
        rsvp "?" *> tipes
   
    tipes <- rule $
        concat <$> some tipe
   
    tipe <- rule $
        mono tipeId <|> tipeGroup
        
    tipeGroup <- rule $
        concat <$> parens (many tipe)
    
    tipeId <- rule $ 
            varId
        <|> conId
        <|> opId
        <|> mixfixId
        <|> rsvp "->"
        <|> rsvp "<-"
        <|> rsvp "."
        <|> rsvp ","
        <|> rsvp "["
        <|> rsvp "]"
        <|> rsvp "_"

-- -----------------------------------------------------------------------------
-- Type Declaration Rules

    typeDecl <- rule $
        TD.TypeDecl <$> typeCtx0 <*> typeDeclCons <*> typeDeclArgs0
        
    typeDeclCons <- rule $
        conName <|> mixfixName
      
    typeDeclArgs0 <- rule $
         many typeDeclArg
        
    typeDeclArgs <- rule $
        some typeDeclArg
      
    typeDeclArg <- rule $
        mono typeDeclArgId <|> rawParens

    typeDeclArgId <- rule $
        varId <|> conId <|> mixfixId <|> parens opId
        
    
-- -----------------------------------------------------------------------------
-- Type Alias Rules

    aliasDef <- rule $
      AD.AliasDef <$> typeDecl <*> (rsvp "-" *> raw)
      

-- -----------------------------------------------------------------------------
-- Data Definition Rules

    dataDef <- rule $
      dataDefA <|> dataDefB
      
    dataDefA <- rule $
      DD.DataDef <$> typeDecl <*> dataDefABody
      
    dataDefB <- rule $
      DD.mkRecDef <$> typeDecl <*> dataDefBBody
    
    dataDefABody <- rule $
      rsvp ":-" *> block dataCons
      
    dataDefBBody <- rule $
      rsvp ":-" *> block dataRow
      
    
    dataCons <- rule $
      dataConsA <|> dataConsB
    
    dataConsA <- rule $
      DD.DataCons <$> conName <*> pure [] <*> dataConsBody
      
    dataConsB <- rule $
      DD.DataCons <$> conName <*> typesig <*> pure []
      
    dataConsType <- rule $
      rsvp "?" *> raw
      
      
    dataConsBody <- rule $
      dataConsBodyA <|> dataConsBodyB
      
    
    dataConsBodyA <- rule $
      rsvp ":" *> block dataRow
    
    dataRow <- rule $
      DD.DataRow <$> (Just <$> varName) <*> dataConsType
    
    
    dataConsBodyB <- rule $
      some dataConsArg
    
    dataConsArg <- rule $
       DD.DataRow Nothing <$> typeDeclArg
    
    
-- -----------------------------------------------------------------------------
-- Type Class Definition Rules

    typeClassDef <- rule $
      TCD.TypeClassDef <$> typeDecl <*> typeClassDefBody
    
    typeClassDefBody <- rule $
      rsvp ":~" *> block exprDef
      
      
-- -----------------------------------------------------------------------------
-- Raw Token Rules
      
    raw0 <- rule $
      raw <|> pure []
    
    raw <- rule $
      many notLayout <|> rawBlk <|> rawLn <|> rawParens
      
    rawBlk <- rule $
      surroundList <$> blk <*> raw0 <*> blk'
    
    rawLn <- rule $
      surroundList <$> ln <*> raw0 <*> ln'
      
    rawParens <- rule $
      surroundList <$> rsvp "(" <*> raw0 <*> rsvp ")"


{- Types are parsed using a different grammar
-- -----------------------------------------------------------------------------
-- Type expressions
    typesig0 <- rule $
      (Just <$> typesig) <|> pure Nothing
    
    typesig <- rule $ rsvp "::" *> typ
    
    typ <- mixfixExpressionSeparate typOps typ'
    
    typ' <- rule $ parens typApp <|> typApp
    
    typApp <- rule $ 
      Ty.apply <$> typVal <*> many typVal
    
    typVal <- rule $ tyTuple <|> typCon <|> typUnit
      
    tyTuple <- rule $ Ty.tuple <$> (parens $ sep (rsvp ",") typApp)
        
    typCon <- rule $ Ty.Con <$> conName
    
    typUnit <- rule $ rsvp "(" *> rsvp ")" *> pure Ty.unit
-}


{- Expressions are another type of parser
-- -----------------------------------------------------------------------------
-- Statement Rules   
    stmtblock <- rule $ block statement
    
    statement <- rule $
          stmtRet
      <|> stmtCall
      <|> stmtAssign
      <|> stmtVarBind
      
    stmtCall <- rule $ 
      Stmt.Call <$> fexpr
    
    stmtVarBind <- rule $ 
      Stmt.Let <$> var
    
    stmtAssign <- rule $
      Stmt.Assign <$> varName <*> typesig0 <*> (op "=" *> expr)
    
    stmtRet <- rule $
      Stmt.Return <$> (rsvp "return" *> expr)
    

-- -----------------------------------------------------------------------------
-- Expression Rules
    expr <- mixfixExpressionSeparate exprOps expr'

    expr' <- rule $
          exprTyped
      <|> expr0
    
    
    exprTyped <- rule $
      E.Cast <$> expr0 <*> typesig
    
    
    expr0 <- rule $
          fexpr
      <|> aexpr
    
    
    fexpr <- rule $
        E.App <$> bexpr <*> many aexpr
    
    aexpr <- rule $
          litExpr
      <|> bexpr
      
    bexpr <- rule $ 
          varExpr
      <|> conExpr
      <|> nestedExpr
    
    varExpr <- rule $ E.Var <$> varName
    conExpr <- rule $ E.Con <$> conName
    litExpr <- rule $ E.Lit <$> lit
    nestedExpr <- rule $ parens expr
    
-}
    
    
    return modl