{-# LANGUAGE TypeSynonymInstances #-}
module Language.Hawk.Syntax.ModuleName where

import Data.Binary
import Data.Data
import Data.Text.Lazy (Text)
import Data.Typeable

import qualified Data.Text.Lazy as Text
import qualified Language.Hawk.Compile.Package as Package
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type Raw = [Text]
  
data Name
  = Name
    { _package  :: Package.Name
    , _module   :: Raw
    } deriving(Eq, Ord, Show, Data, Typeable)
    
    
inCore :: [Text] -> Name
inCore raw =
  Name Package.core raw
  
  
toStringRaw :: Raw -> String
toStringRaw = Text.unpack . Text.intercalate "."
      
toString :: Name -> String
toString (Name _ n) = toStringRaw n


instance PP.Pretty Name where
    pretty =
      PP.pretty . toString


instance Binary Name where
  put (Name pkg name) =
    put pkg >> put name
    
  get =
    Name <$> get <*> get