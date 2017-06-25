{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Report.SrcLoc where

import Control.Lens
import Data.Binary
import Data.Text.Lazy (pack)
import Language.Hawk.Report.Region
import Text.PrettyPrint.Leijen.Text ((<>))

import qualified Text.PrettyPrint.Leijen.Text as PP


data SrcLoc
  = SrcLoc
    { _srcPath :: FilePath
    , _srcReg :: Region 
    }
    deriving (Eq, Ord, Show)

makeClassy ''SrcLoc

instance HasRegion SrcLoc where
    region = srcReg . region


instance PP.Pretty SrcLoc where
    pretty loc =
       PP.text (pack $ loc^.srcPath) <> PP.text ":" <> PP.pretty (loc^.srcReg)


instance Binary SrcLoc where
    get =
        SrcLoc <$> get <*> get
        
    put (SrcLoc p r) =
        put p >> put r