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
    varName <- rule $ varId
    
    conName <- rule $ conId
    
    opName <- rule $ opId
    
    name <- rule $ varName <|> conName


-- -----------------------------------------------------------------------------
-- Module path rules
    paths0 <- rule $
      paths <|> pure []

    paths <- rule $
      ((:) <$> paths_base <*> paths0)
    
    paths_base <- rule $
          (Node <$> conName <*> paths_sub)
      <|> (Node <$> (conName <|> varName <|> parens opName)
                <*> pure []
          )
    
    paths_sub <- rule $  
          ((:[]) <$> (op "." *> paths_base))
      <|> (rsvp "(" *> many paths_base <* rsvp ")")


-- -----------------------------------------------------------------------------
-- Item Rules
    items <- rule $
      linefolds item

    item <- rule $
          impItem
      <|> expItem
      <|> exprDefItem
      <|> aliasDefItem
      <|> dataDefItem
      <|> typeClassDefItem
    
    
    impItem <- rule $
      I.Import <$> (op "->" *> paths)
      
    expItem <- rule $
      I.Export <$> (op "<-" *> paths)
      
    exprDefItem <- rule $
      I.ExprDef <$> exprDef
      
    aliasDefItem <- rule $
      I.AliasDef <$> aliasDef  
      
    dataDefItem <- rule $
      I.DataDef <$> dataDef
      
    typeClassDefItem <- rule $
      I.TypeClassDef <$> typeClassDef
      

-- -----------------------------------------------------------------------------
-- Type Context Rules

    typeCtx0 <- rule $
        typeCtx <|> pure QT.emptyCtx
       
    typeCtx <- rule $
        (QT.Context <$> typeCtxRaw) <* op "=>"
      
    typeCtxRaw <- rule $
        (:[]) <$> many notTypeCtxArr


-- -----------------------------------------------------------------------------
-- Expression Definition Rules
    exprDecl <- rule $
        EDec.ExprDecl <$> exprDefName <*> opInfo0 <*> many exprVar <*> exprType0
    
    exprDef <- rule $
        EDef.mkExprDef <$> exprDecl <*> exprDefBody

    exprDefName <- rule $
        varName <|> parens opName
    
    exprDefOp <- rule $
        op "=" <|> op ":=" <|> rsvp ":"
     
    
    exprVar <- rule $
        varName <|> opNamed "_"
    
    
    exprType0 <- rule $
      exprType <|> pure []
      
    exprType <- rule $
        op "?" *> many (notTokens [ Lex.TokenRsvp ":"
                                  , Lex.TokenOpId "="
                                  , Lex.TokenOpId ":="
                                  ]
                        )
    
    exprDefBody <- rule $
        exprDefOp *> raw
    
        
    opInfo0 <- rule $
        opInfo <|> pure OI.defOpInfo
    
    opInfo <- rule $
        OI.OpInfo <$> tInteger <*> (OI.assocFromName <$> varId)


-- -----------------------------------------------------------------------------
-- Type Declaration Rules

    typeDecl <- rule $
        TD.TypeDecl <$> typeCtx0 <*> conName <*> typeDeclArgs
      
    typeDeclArgs <- rule $
        many typeDeclArg
      
    typeDeclArg <- rule $
        rawParens <|> (some (varTok <|> conTok))
    
-- -----------------------------------------------------------------------------
-- Type Alias Rules

    aliasDef <- rule $
      AD.AliasDef <$> typeDecl <*> (op "-" *> raw)
      

-- -----------------------------------------------------------------------------
-- Data Definition Rules

    dataDef <- rule $
      dataDefA <|> dataDefB
      
    dataDefA <- rule $
      DD.DataDef <$> typeDecl <*> dataDefABody
      
    dataDefB <- rule $
      DD.mkRecDef <$> typeDecl <*> dataDefBBody
    
    dataDefABody <- rule $
      op ":-" *> block dataCons
      
    dataDefBBody <- rule $
      op ":-" *> block dataConsRowA
      
    
    dataCons <- rule $
      dataConsA <|> dataConsB
    
    dataConsA <- rule $
      DD.DataCons <$> conName <*> pure [] <*> dataConsBody
      
    dataConsB <- rule $
      DD.DataCons <$> conName <*> dataConsType <*> pure []
      
    dataConsType <- rule $
      (op "?" *> raw)
      
      
    dataConsBody <- rule $
      dataConsBodyA <|> dataConsBodyB
      
    
    dataConsBodyA <- rule $
      rsvp ":" *> block dataConsRow
    
    dataConsRowA <- rule $
      (,) <$> (Just <$> varName) <*> (op "?" *> raw)
    
    
    dataConsBodyB <- rule $
      some dataConsBodyB
    
    dataConsRowB <- rule $
      (Nothing,) <$> typeDeclArg
    
    
-- -----------------------------------------------------------------------------
-- Type Class Definition Rules

    typeClassDef <- rule $
      TCD.TypeClassDef <$> typeDecl <*> typeClassDefBody
    
    typeClassDefBody <- rule $
      rsvp ":~" *> block exprDef
      
      
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

      
      
-- -----------------------------------------------------------------------------
-- Raw Token Rules


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