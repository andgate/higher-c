{-# LANGUAGE ExistentialQuantification
           , RankNTypes
           , OverloadedStrings
           , TypeFamilies
  #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Control.Monad.Trans.State.Strict (evalState)
import Data.Monoid
import Data.Text.Buildable (Buildable(..))
import Data.Text (Text)
import Data.Word (Word8)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Report.Region (region)
import Language.Hawk.Report.SrcLoc
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Operator
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)
import Text.Earley
import Text.Earley.Mixfix

import qualified Data.ByteString.UTF8             as UTF8
import qualified Data.Text                        as T
import qualified Language.Hawk.Report.Region      as R

-- -----------------------------------------------------------------------------
-- Parser type
--type HkProd r a = Prod r Token Token a
--type HkGrammar r a = Grammar r (Prod r Token Token a)
        
type OpTable r a = [[(Holey (Prod r Token Token Token), Associativity, Holey Token -> [a] -> a)]]
--type TypeOpTable r = OpTable r Ty.Typed
type ExprOpTable r = OpTable r ExpPs

-- -----------------------------------------------------------------------------
-- Mixfix
exprOpTable :: ExprOpTable r
exprOpTable =
  [ [assign]
  , [lt, gt, leq, geq]
  , [binadd, binsub]
  , [postAccess]
  -- , [(holey "_$_", RightAssoc, typDollar)]
  ]
  where
    assign = ([Nothing, Just (rsvp "="), Nothing], RightAssoc, \ _ [l,r] -> EAssign () l AssignOp r)
    
    lt = ([Nothing, Just (rsvp "<"), Nothing], NonAssoc, \ _ [l,r] -> EBinary () l LeOp r)
    gt = ([Nothing, Just (rsvp ">"), Nothing], NonAssoc, \ _ [l,r] -> EBinary () l GrOp r)
    leq = ([Nothing, Just (op "<="), Nothing], NonAssoc, \ _ [l,r] -> EBinary () l LeqOp r)
    geq = ([Nothing, Just (op ">="), Nothing], NonAssoc, \ _ [l,r] -> EBinary () l GeqOp r)

    binadd = ([Nothing, Just (op "+"), Nothing], LeftAssoc, \ _ [l,r] -> EBinary () l AddOp r)
    binsub = ([Nothing, Just (op "-"), Nothing], LeftAssoc, \ _ [l,r] -> EBinary () l SubOp r)

    postAccess = ([Nothing, Just (rsvp "."), Nothing], LeftAssoc, \ _ [a, b] -> EBinary () a AccessOp b)

{- 
defTypeOps :: TypeOpTable
defTypeOps =
  [ [(holey "_->_", RightAssoc, typArrow)]
  , [(holey "_$_", RightAssoc, typDollar)]
  ]

  
typArrow :: Holey Token -> [Ty.Typed] -> Ty.Typed
typArrow _ = Ty.typeCon "_->_"

typParens :: Holey Token -> [Ty.Typed] -> Ty.Typed
typParens _ = Ty.typeCon "(_)"

typDollar :: Holey Token  -> [Ty.Typed] -> Ty.Typed
typDollar _ = Ty.typeCon "_$_"

-}

-- -----------------------------------------------------------------------------
-- Terminal Production Helpers
match :: TokenClass -> Prod r e Token Token
match c = satisfy p
  where p (Token c' _ _ _) = c == c'

rsvp :: Text -> Prod r e Token Token
rsvp =
  match . TokenRsvp

prim :: Text -> Prod r e Token Token
prim =
  match . TokenPrim

op :: Text -> Prod r e Token Token
op =
  match . TokenOpId

-- -----------------------------------------------------------------------------
-- Combinator Helpers

parens :: Prod r e Token a -> Prod r e Token a
parens p =
  rsvp "(" *> p <* rsvp ")"

sqrBrackets :: Prod r e Token a -> Prod r e Token a
sqrBrackets p =
  rsvp "[" *> p <* rsvp "]"

curlyBrackets :: Prod r e Token a -> Prod r e Token a
curlyBrackets p =
  rsvp "{" *> p <* rsvp "}"

angleBrackets :: Prod r e Token a -> Prod r e Token a
angleBrackets p =
  rsvp "<" *> p <* rsvp ">"

           
sep :: Prod r e Token b -> Prod r e Token a -> Prod r e Token [a]
sep s p =
  (:) <$> p <*> many (s *> p)
  
sep' :: Prod r e Token b -> Prod r e Token a -> Prod r e Token [a]
sep' s p =
  sep s p <|> pure []
  
mono :: Prod r e Token a -> Prod r e Token [a]
mono =
  fmap (:[])
  
prepend :: Prod r e Token a -> Prod r e Token [a] -> Prod r e Token [a]  
prepend =
  liftA2 (:)

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Name Tokens

varName :: Prod r e Token NamePs
varName = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenVarId _) _ _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenVarId n) _ fp r) = Name n (SrcLoc fp r)

conName :: Prod r e Token NamePs
conName = modName <|> conName'

conName' :: Prod r e Token NamePs
conName' = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenConId _) _ _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenConId n) _ fp r) = Name n (SrcLoc fp r)


modName :: Prod r e Token NamePs
modName = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenModId _) _ _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenModId n) _ fp r) = Name n (SrcLoc fp r)


opName :: Prod r e Token NamePs
opName = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenOpId _) _ _ _) = True
    p  _                        = False
    unsafeExtract (Token (TokenOpId n) _ fp r) = Name n (SrcLoc fp r)


-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Literal Tokens
tInteger :: Prod r e Token Integer
tInteger = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenInteger _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenInteger v) _ _ _) = v

tReal :: Prod r e Token Double
tReal = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenDouble _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenDouble v) _ _ _) = v

tChar :: Prod r e Token Char
tChar = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenChar _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenChar v) _ _ _) = v

tString :: Prod r e Token String
tString = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenString _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenString v) _ _ _) = v

tBool :: Prod r e Token Bool
tBool = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenBool _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenBool v) _ _ _) = v


-- -----------------------------------------------------------------------------
-- Layout Helpers

eof :: Prod r e Token Token
eof = match TokenEof

block :: Prod r e Token a -> Prod r e Token [a]
block p = blk *> linefolds p <* blk'

linefolds0 :: Prod r e Token a -> Prod r e Token [a]
linefolds0 p = many $ linefold p

linefolds :: Prod r e Token a -> Prod r e Token [a]
linefolds p = some $ linefold p

linefold :: Prod r e Token a -> Prod r e Token a
linefold p = ln *> p <* ln'


blk :: Prod r e Token Token
blk = match TokenBlk

blk' :: Prod r e Token Token
blk' = match TokenBlk'


ln :: Prod r e Token Token
ln = match TokenLn

ln' :: Prod r e Token Token
ln' = match TokenLn'


-- -----------------------------------------------------------------------------
-- Syntax Constructor Helpers

mkUnitTyCon :: Token -> Token -> TypePs
mkUnitTyCon t1 t2 =
    TyCon () $
      Name  "()"
            (SrcLoc (t1^.tokFilepath) (t1^.region <> t2^.region))

mkFunTyCon :: Token -> Token -> Token -> TypePs
mkFunTyCon t1 t2 t3 =
    TyCon () $
      Name  "->"
            (SrcLoc (t1^.tokFilepath) (t1^.region <> t3^.region))

mkListTyCon :: Token -> Token -> TypePs
mkListTyCon t1 t2 =
    TyCon () $
      Name  "[]"
            (SrcLoc (t1^.tokFilepath) (t1^.region <> t2^.region))

mkTupleTyCon :: Token -> [Token] -> Token -> TypePs
mkTupleTyCon t1 t2s t3 =
    TyCon () $
      Name  (T.pack ("Tuple" ++ (show . length) t2s))
            (SrcLoc (t1^.tokFilepath) (t1^.region <> t3^.region))