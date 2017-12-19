{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.TypeCheck.Environment where

import Control.Arrow
import Control.Lens
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Monoid
import Language.Hawk.Syntax hiding (fromSig)

import qualified Data.Map.Strict as Map


-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env = TypeEnv
  { _types :: Map Text Type 
  , _constr :: Map Text Type
  }
  deriving (Eq, Show)


makeClassy ''Env


instance Monoid Env where
  mempty = empty
  mappend = merge
  

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

empty :: Env
empty = TypeEnv Map.empty Map.empty


extendType :: HasEnv e => e -> (Text, Type) -> e
extendType e (n, t) =
  e & env . types %~ Map.insert n t

extendCon :: HasEnv e => e -> (Text, Type) -> e
extendCon e (n, t) =
  e & env . constr %~ Map.insert n t


removeType :: HasEnv e => e -> Text -> e
removeType e n =
  e & env . types %~ Map.delete n


removeCon :: HasEnv e => e -> Text -> e
removeCon e n =
  e & env . constr %~ Map.delete n


extendTypes :: HasEnv e => e -> [(Text, Type)] -> e
extendTypes e ns =
  e & env . types %~ Map.union (Map.fromList ns)


extendCons :: HasEnv e => e -> [(Text, Type)] -> e
extendCons e ns =
  e & env . constr %~ Map.union (Map.fromList ns)


lookupType :: HasEnv e => Text -> e -> Maybe Type
lookupType n e =
  Map.lookup n $ e ^. env . types


lookupCon :: HasEnv e => Text -> e -> Maybe Type
lookupCon n e =
  Map.lookup n $ e ^. env . constr


merge :: HasEnv e => e -> e -> e
merge e1 e2 =
  e1 & env . types %~ Map.union (e2 ^. env . types)
     & env . constr %~ Map.union (e2 ^. env . constr)


mergeMany :: HasEnv e => [e] -> Env
mergeMany = foldr (merge . view env) empty


mergeSome :: HasEnv e => [e] -> e
mergeSome = foldr1 merge


fromType :: Text -> Type -> Env
fromType n t =
  empty & types .~ Map.singleton n t

fromTypes :: [(Text, Type)] -> Env
fromTypes = mconcat . map (uncurry fromType)


fromSig :: Sig -> Env
fromSig (Sig n t) = fromType n t

fromSigs :: [Sig] -> Env
fromSigs = mconcat . map fromSig


fromCon :: Text -> Type -> Env
fromCon n t =
  empty & constr .~ Map.singleton n t

fromCons :: [(Text, Type)] -> Env
fromCons = mconcat . map (uncurry fromCon)


typeNames :: HasEnv e => e -> [Text]
typeNames e =
  Map.keys (e ^. env . types)

conNames :: HasEnv e => e -> [Text]
conNames e =
  Map.keys (e ^. env . constr)


toTypes :: HasEnv e => e -> [(Text, Type)]
toTypes = Map.toList . view (env . types)

toSigs :: Env -> [Sig]
toSigs env = uncurry Sig <$> toTypes env

toCons :: HasEnv e => e -> [(Text, Type)]
toCons = Map.toList . view (env . constr)


fromImg :: Image -> Env
fromImg img = fromSigs sigs <> fromCons cons
  where
    sigs = sigs1 <> sigs2 <> sigs3
    sigs1 = img^.imgSigs
    sigs2 = concatMap structSigs (img^. imgTStructs)
    sigs3 = catMaybes $ foreignSig <$> (img^.imgForeign)
    cons = (structTName &&& structType) <$> (img^. imgTStructs)