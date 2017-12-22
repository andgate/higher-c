{-# LANGUAGE  DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Function where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP


data Fn t p =
  Fn { _fnName :: Text
     , _fnPats :: [p]
     , _fnTerm :: t
     } deriving (Eq, Ord, Show)


instance (PP.Pretty t, PP.Pretty p) => PP.Pretty (Fn t p) where
  pretty (Fn n ps t) =
    PP.pretty n
      PP.<+> PP.pretty ps
      PP.<+> PP.textStrict "="
      PP.<+> PP.pretty t
