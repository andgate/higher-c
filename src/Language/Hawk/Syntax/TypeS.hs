{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.TypeS where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Kind
import Language.Hawk.Syntax.Signature

import qualified Text.PrettyPrint.Leijen.Text as PP


------------------------------------------------------------------------
-- Type Structure
data TypeS
    = TypeS
      { _tsName  :: L Text
      , _tsTVars :: [L Text]
      , _tsCons  :: [TypeCon]
      } deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


data TypeCon
    = TypeCon
      { _dcName :: Name
      , _dcArgs :: [Type]
      }
    | RecCon
      { _dcName :: Name
      , _sumArgs :: [RecLabel]
      }
    deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


data RecLabel =
    RecLabel
    { _rlName :: Name
    , _rlType :: Type
    }
    deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


makeClassy ''TypeS
makeClassy ''TypeCon
makeClassy ''RecLabel

-------------------------------------------------------------------------
-- Helper Instances

-- Binary Serialization
instance Binary TypeS
instance Binary TypeCon
instance Binary RecLabel

-- From JSON
instance FromJSON TypeS
instance FromJSON TypeCon
instance FromJSON RecLabel

-- To JSON
instance ToJSON TypeS
instance ToJSON TypeCon
instance ToJSON RecLabel

-- Plated
instance Plated TypeS
instance Plated TypeCon
instance Plated RecLabel


-- Pretty Printing
instance PP.Pretty TypeS where
    pretty (TypeS n tvs cs) =
      PP.textStrict "data"
        PP.<+> PP.pretty (unL n)
        PP.<+> PP.pretty (unL <$> tvs)
        PP.<+> PP.textStrict "="
        PP.<$> PP.indent 4
            ( foldr (\c d -> PP.pretty c PP.<+> PP.textStrict "|" PP.<+> d) PP.empty cs )


instance PP.Pretty TypeCon where
    pretty = \case
      TypeCon n ts ->
        PP.pretty n PP.<+> PP.pretty ts

      RecCon n fs ->
        PP.pretty n
          PP.<$>
          PP.indent 2 ( PP.braces $
            foldr (\f d -> d PP.<+> PP.pretty f) PP.empty fs
            )


instance PP.Pretty RecLabel where
    pretty (RecLabel n t) =
        PP.pretty n
          PP.<+> PP.textStrict "::"
          PP.<+> PP.pretty t


-------------------------------------------------------------------------
-- Helpers

-- Get term level names, such as constructors and record labels
structNames :: TypeS -> [Text]
structNames (TypeS _ _ b) = concatMap typeConNames b 

typeConNames :: TypeCon -> [Text]
typeConNames = \case
      TypeCon n ts -> [readName n]
      RecCon n ls -> readName n : (readName . _rlName <$> ls)


-- Get type level name of structure
structTName :: TypeS -> Text
structTName = unL . _tsName

-- Get type of struct, wrapped in the kind
structType :: TypeS -> Type
structType s = 
  let n = structTName s
      x = length $ _tsTVars s
  in TKind (kstarN x) $ TCon n

structTVars :: TypeS -> [Type]
structTVars s =
  let ns = unL <$> _tsTVars s
  in TVar <$> ns

-- Get type signatures introduces by struct
structSigs :: TypeS -> [Sig]
structSigs s@(TypeS _ _ cs) =
  let
    t = foldr TApp (structType s) (structTVars s)
  in
    concatMap (typeConSigs t) cs


typeConSigs :: Type -> TypeCon -> [Sig]
typeConSigs t = \case
  TypeCon n ts -> [Sig (readName n) (foldr TArr t ts)]
  RecCon n lbls -> typeRecLSig t <$> lbls


typeRecLSig :: Type -> RecLabel -> Sig
typeRecLSig t1 (RecLabel n t2) = Sig (readName n) (t2 `TArr` t1)
