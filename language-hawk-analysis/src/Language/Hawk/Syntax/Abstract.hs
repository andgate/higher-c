module Language.Hawk.Syntax.Abstract where

import Data.Text
import Data.Map
import Language.Hawk.Syntax.Prim



data Module =
  Module
    { modName     :: Text
    , modVars     :: [Text]
    , modDeps     :: [Text]
    , modGlobals  :: Text
    , modDecls    :: Map Name Def
    , modTypes    :: Map Name Type
    }
  deriving(Show)


data Def
  = Def Text [Text] TermChk
  deriving (Show)


data Name
  = Global String
  | Local Int
  | Quote Int
  deriving(Show, Eq)

data TermChk
  = TAnn TermInf Type
  | TFree Name
  | TBound Int
  | TPrim PrimInstr
  | TApp TermChk TermInf
  deriving (Show, Eq)


data TermInf
  = TInf TermChk
  | TLam TermInf
  deriving (Show, Eq)

data Type
  = TyFree Name
  | TyFun Type Type
  deriving (Show, Eq)

data Kind = KStar
  deriving (Show, Eq)


data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value


vfree :: Name -> Value
vfree = VNeutral . NFree

vapp :: Value -> Value -> Value
vapp v v' = case v of
  VLam f -> f v'
  VNeutral n -> VNeutral (NApp n v')