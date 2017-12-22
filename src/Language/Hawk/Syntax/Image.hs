{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}
module Language.Hawk.Syntax.Image where


import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Fixity
import Language.Hawk.Syntax.Foreign
import Language.Hawk.Syntax.Function
import Language.Hawk.Syntax.Signature
import Language.Hawk.Syntax.DataS

import qualified Text.PrettyPrint.Leijen.Text as PP


data Image t p =
  Image { _imgFns       :: [Fn t p]
        , _imgSigs      :: [Sig t]
        , _imgTStructs  :: [DataS t]
        , _imgFixity    :: [Fixity]
        , _imgForeign   :: [Foreign t]
        } deriving (Show, Eq)

makeLens ''Image


instance Monoid (Image t p) where
  mempty
    = Image
      { _imgFns       = []
      , _imgSigs      = []
      , _imgTStructs  = []
      , _imgFixity    = []
      , _imgForeign   = []
      }

  mappend m1 m2 =
    m1 & (imgFns     %~ (++ m2^.imgFns))
      . (imgSigs    %~ (++ m2^.imgSigs))
      . (imgTStructs %~ (++ m2^.imgTStructs))
      . (imgFixity  %~ (++ m2^.imgFixity))
      . (imgForeign %~ (++ m2^.imgForeign))


instance Default (Image t p) where
  def = mempty


instance (PP.Pretty t, PP.Pretty p) => PP.Pretty (Image t p) where
  pretty m =
    PP.textStrict "Functions:" PP.<$> PP.pretty (m^.imgFns)
    PP.<$> PP.textStrict "Signatures:" PP.<$> PP.pretty (m^.imgSigs)
    PP.<$> PP.textStrict "Type Structs" PP.<+> PP.pretty (m^.imgTStructs)
    PP.<$> PP.textStrict "Fixities" PP.<+> PP.pretty (m^.imgFixity)
    PP.<$> PP.textStrict "Foreign" PP.<+> PP.pretty (m^.imgForeign)
    


fromFn :: Fn t p -> Image t p
fromFn f = mempty & imgFns .~ [f]

fromSig :: Sig t -> Image t p
fromSig s = mempty & imgSigs .~ [s]

fromTStruct :: DataS t -> Image t p
fromTStruct d = mempty & imgTStructs .~ [d]

fromFixity :: Fixity t -> Image t p
fromFixity d = mempty & imgFixity .~ [d]

fromForeign :: Foreign t -> Image t p
fromForeign d = mempty & imgForeign .~ [d]
