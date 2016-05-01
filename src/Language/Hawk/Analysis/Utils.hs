module Language.Hawk.Analysis.Utils where

import Data.List
import Data.Data
import Data.Generics.Uniplate.Data
import Language.Hawk.Syntax.AST

evalType :: HkType a -> HkType a
evalType t = undefined

mkFnParam :: HkType a -> (String, HkType a)
mkFnParam t =  undefined

findExtFns :: (Data.Data.Data a) => HkTranslUnit a -> [HkExtStmt a]
findExtFns ast
  = [fn | fn@(HkExtFnDef _ _ _) <- universeBi ast]