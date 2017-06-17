{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Report.SrcLoc where

import Control.Lens
import Data.Binary
import Language.Hawk.Report.Region
import Text.PrettyPrint.ANSI.Leijen ((<>))

import qualified Text.PrettyPrint.ANSI.Leijen as PP


data SrcLoc
  = SrcLoc
    { _srcPath :: FilePath
    , _srcReg :: Region 
    }
    deriving (Eq, Ord, Show)

makeLenses ''SrcLoc

instance HasRegion SrcLoc where
    region = srcReg


instance PP.Pretty SrcLoc where
    pretty loc =
       PP.text (loc^.srcPath) <> PP.text ":" <> PP.pretty (loc^.srcReg)


instance Binary SrcLoc where
    get =
        SrcLoc <$> get <*> get
        
    put (SrcLoc p r) =
        put p >> put r