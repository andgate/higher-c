{-# LANGUAGE  TemplateHaskell, DeriveGeneric, OverloadedStrings  #-}
module Language.Hawk.Syntax.Image where


import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.DataS
import Language.Hawk.Syntax.Fixity
import Language.Hawk.Syntax.Foreign
import Language.Hawk.Syntax.Function
import Language.Hawk.Syntax.Signature
import Language.Hawk.Syntax.TypeAlias
import Language.Hawk.Syntax.TypeDef

import qualified Text.PrettyPrint.Leijen.Text as PP


data Image =
  Image { _imgFns     :: [Fn]
        , _imgSigs    :: [Sig]
        , _imgTDefs   :: [TypeDef]
        , _imgTAlias  :: [TypeAlias]
        , _imgDataS   :: [DataS]
        , _imgFixity  :: [Fixity]
        , _imgForeign :: [Foreign]
        } deriving (Show, Eq, Generic)

makeClassy ''Image


instance Binary Image
instance FromJSON Image
instance ToJSON Image


instance Monoid Image where
  mempty
    = Image
      { _imgFns     = []
      , _imgSigs    = []
      , _imgTDefs   = []
      , _imgTAlias  = []
      , _imgDataS   = []
      , _imgFixity  = []
      , _imgForeign = []
      }

  mappend m1 m2 =
    m1 & (imgFns     %~ (++ m2^.imgFns))
       . (imgSigs    %~ (++ m2^.imgSigs))
       . (imgTAlias  %~ (++ m2^.imgTAlias))
       . (imgTDefs   %~ (++ m2^.imgTDefs))
       . (imgDataS   %~ (++ m2^.imgDataS))
       . (imgFixity  %~ (++ m2^.imgFixity))
       . (imgForeign %~ (++ m2^.imgForeign))


instance Default Image where
  def = mempty


instance PP.Pretty Image where
  pretty m =
    PP.textStrict "Functions:" PP.<$> PP.pretty (m^.imgFns)
    PP.<$> PP.textStrict "Signatures:" PP.<$> PP.pretty (m^.imgSigs)
    PP.<$> PP.textStrict "Type Alias:" PP.<$> PP.pretty (m^.imgTAlias)
    PP.<$> PP.textStrict "Type Definitions" PP.<$> PP.pretty (m^.imgTDefs)
    PP.<$> PP.textStrict "Data Structs" PP.<+> PP.pretty (m^.imgDataS)
    PP.<$> PP.textStrict "Fixities" PP.<+> PP.pretty (m^.imgFixity)
    PP.<$> PP.textStrict "Foreign" PP.<+> PP.pretty (m^.imgForeign)
    


fromFn :: Fn -> Image
fromFn f = mempty & imgFns .~ [f]

fromSig :: Sig -> Image
fromSig s = mempty & imgSigs .~ [s]

fromTAlias :: TypeAlias -> Image
fromTAlias d = mempty & imgTAlias .~ [d]

fromTDef :: TypeDef -> Image
fromTDef d = mempty & imgTDefs .~ [d]

fromData :: DataS -> Image
fromData d = mempty & imgDataS .~ [d]

fromFixity :: Fixity -> Image
fromFixity d = mempty & imgFixity .~ [d]

fromForeign :: Foreign -> Image
fromForeign d = mempty & imgForeign .~ [d]