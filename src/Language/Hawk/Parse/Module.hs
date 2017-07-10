{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
  #-}
module Language.Hawk.Parse.Module where

import Control.Applicative
import Control.Lens
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, pack)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax


import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

-- Items that this parser will produce and store in the top-level module
data ModItem
  = SubMod SrcMod 
  | ModDep Dependency
  | OpDec [Operator]
  | UnparsedItem [Token]



modP :: MonadParser m => FilePath -> m SrcMod
modP fp =
  mkMod fp <$> linefold modHeader
           <*> linefolds (modItem fp)


subModP :: MonadParser m
        => FilePath -> m SrcMod
subModP fp =
  mkMod fp <$> modHeader
           <*> subModBody fp

modHeader :: MonadParser m => m [Text]
modHeader =
  P.try (rsvp "mod") >> modPath


subModBody :: MonadParser m
           => FilePath -> m [ModItem]
subModBody fp =
  rsvp ":=" *> (block $ modItem fp)


modItem :: MonadParser m
        => FilePath -> m ModItem
modItem fp =
      (SubMod <$> subModP fp)
 -- <|> (ModDep <$> dep)
  <|> (OpDec <$> infixDec)
  <|> (UnparsedItem <$> anyLayout)



infixDec :: MonadParser m
         => m [Operator]
infixDec =
  infixDecN <|> infixDecL <|> infixDecR

infixDecN :: MonadParser m
          => m [Operator]
infixDecN = do
  P.try $ rsvp "infix"
  p <- integerP
  checkFixity p
  ops <- some anyOpNameP
  return $ map (Op InfixN p) ops


infixDecL :: MonadParser m
          => m [Operator]
infixDecL = do
  P.try $ rsvp "infixl"
  p <- integerP
  checkFixity p
  ops <- some anyOpNameP
  return $ map (Op InfixL p) ops

infixDecR :: MonadParser m
          => m [Operator]
infixDecR = do
  P.try $ rsvp "infixr"
  p <- integerP
  checkFixity p
  ops <- some anyOpNameP
  return $ map (Op InfixR p) ops


-- This suggests I need to implement custom megaparsec errors
checkFixity :: MonadParser m => Integer -> m ()
checkFixity x
  | x < 0 = error "fixity cannot be less than 0"
  | x > 9 = error "fixity cannot be greater than 9"
  | otherwise = return ()


modPath :: MonadParser m => m [Text]
modPath =
  modPath' <|> (pure <$> anyModId)

modPath' :: MonadParser m => m [Text]
modPath' =
  (:) <$> anyModId <*> modPathNext

modPathNext :: MonadParser m => m [Text]
modPathNext =
  (rsvp "." *> modPath') <|> return []



-- -----------------------------------------------------------------------------
-- | Module Specific Helpers

-- Extraction -----------------------------------------------------------------
extractSubs :: [ModItem] -> [SrcMod]
extractSubs = foldr extract []
  where extract (SubMod m) ms = m:ms
        extract _ ms = ms


extractDeps :: [ModItem] -> [Dependency]
extractDeps = foldr extract []
  where extract (ModDep d) ds = d:ds
        extract _ ds = ds

extractOps :: [ModItem] -> [Operator]
extractOps = foldr extract []
  where extract (OpDec ops1) ops2 = ops1 ++ ops2
        extract _ ops = ops

extractToks :: [ModItem] -> [[Token]]
extractToks = foldr extract []
  where extract (UnparsedItem ts) tss = ts:tss
        extract _ tss = tss


mkOpTable :: [Operator] -> Map OpName Operator
mkOpTable ops
  = Map.fromList (zip opNames ops)
  where
    opNames = map _opName ops


-- Super Duper Smart Constructor --------------------------------------------

-- Another example of why I need custom MegaParsec errors!!
mkMod :: FilePath -> [Text] -> [ModItem] -> SrcMod
mkMod fp [] _ = error "Cannot make empty module!"
mkMod fp [n] xs
  = insertModsWith mappend subs m
  where
    subs = extractSubs xs
    deps = extractDeps xs
    ops = mkOpTable $ extractOps xs
    toks = extractToks xs

    m = SrcMod { _modName = n
            , _modSubs = mempty
            , _modScopes = Map.singleton (pack fp) ms
            }

    ms = mempty
            { _mscopePath  = fp
            , _mscopeItems = mempty
            , _mscopeTabs  = mstabs
            , _mscopeToks  = toks
            }
    mstabs = def { _mscopeOps = ops, _mscopeDeps = deps }

mkMod fp (n:ns) xs
  = SrcMod { _modName   = n
        , _modSubs   = Map.singleton (head ns) (mkMod fp ns xs)
        , _modScopes = mempty
        }


