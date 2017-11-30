{-# LANGUAGE  DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
module Language.Hawk.Syntax.Function where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Expression
import Language.Hawk.Syntax.Type


import qualified Text.PrettyPrint.Leijen.Text as PP


data Fn =
  Fn { _fnName :: Text
     , _fnArgs :: [(Text, Type)]
     , _fnBody :: Exp
     } deriving (Show, Eq, Generic)


makeClassy ''Fn


instance Binary Fn
instance FromJSON Fn
instance ToJSON Fn


instance PP.Pretty Fn where
  pretty (Fn n xs e) =
    PP.textStrict n
      PP.<+> spaceSep ns
      PP.<+> PP.textStrict "="
      PP.<+> PP.pretty e

    where
      ns = map (PP.textStrict . fst) xs
      spaceSep = PP.encloseSep PP.empty PP.empty (PP.char ' ')
