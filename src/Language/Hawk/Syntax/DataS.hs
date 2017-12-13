{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.DataS where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Type

import qualified Text.PrettyPrint.Leijen.Text as PP


------------------------------------------------------------------------
-- Data Structure
data DataS
    = DataS
      { _dataName  :: L Text
      , _dataTVars :: [L Text]
      , _dataBody  :: [DataCon]
      } deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


data DataCon
    = DataCon
      { _dcName :: Name
      , _dcArgs :: [Type]
      }
    | DataRec
      { _dcName :: Name
      , _sumArgs :: [RecField]
      }
    deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


data RecField =
    RecField Name Type
    deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


makeClassy ''DataS
makeClassy ''DataCon
makeClassy ''RecField

-------------------------------------------------------------------------
-- Helper Instances

-- Binary Serialization
instance Binary DataS
instance Binary DataCon
instance Binary RecField

-- From JSON
instance FromJSON DataS
instance FromJSON DataCon
instance FromJSON RecField

-- To JSON
instance ToJSON DataS
instance ToJSON DataCon
instance ToJSON RecField

-- Plated
instance Plated DataS
instance Plated DataCon
instance Plated RecField


-- Pretty Printing
instance PP.Pretty DataS where
    pretty (DataS n tvs body) =
      PP.textStrict "data"
        PP.<+> PP.pretty (unL n)
        PP.<+> PP.pretty (unL <$> tvs)
        PP.<+> PP.textStrict "="
        PP.<$> PP.indent 4
            ( foldr (\c d -> PP.pretty c PP.<+> PP.textStrict "|" PP.<+> d) PP.empty body )


instance PP.Pretty DataCon where
    pretty = \case
      DataCon n ts ->
        PP.pretty n PP.<+> PP.pretty ts

      DataRec n fs ->
        PP.pretty n
          PP.<$>
          PP.indent 2 ( PP.braces $
            foldr (\f d -> d PP.<+> PP.pretty f) PP.empty fs
            )


instance PP.Pretty RecField where
    pretty (RecField n t) =
        PP.pretty n
          PP.<+> PP.textStrict "::"
          PP.<+> PP.pretty t


-------------------------------------------------------------------------
-- Helpers


class HasNames a where
  names :: a -> [Text]



instance HasNames DataS where
  names d = n:ns
    where
      n = unL (d^.dataName)
      ns = concatMap names (d^.dataBody)

  
instance HasNames DataCon where
  names = \case
    DataCon n _ -> [readName n]

    DataRec n args -> n':ns
      where
        n' = readName n
        ns = concatMap names args


instance HasNames RecField where
  names (RecField n _) = [readName n]
