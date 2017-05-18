module Language.Hawk.Syntax.Module where

import Data.Data
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))


import qualified Data.Map                               as Map
import qualified Data.Text                              as Text
import qualified Language.Hawk.Syntax.Item              as I
import qualified Language.Hawk.Syntax.Expression        as E
import qualified Language.Hawk.Syntax.Type              as T
import qualified Language.Hawk.Syntax.Name              as N
import qualified Text.PrettyPrint.ANSI.Leijen           as PP


type Source =
  Module N.Source E.Source T.Source
  
type Valid =
  Module N.Valid E.Valid T.Valid
  
type Typed =
  Module N.Typed E.Typed T.Typed
   
data Module n e t
  = Module 
    { modName   :: Text
    , modPath   :: [Text]
    , modItems  :: [I.Item n e t]
    } deriving(Eq, Show, Data, Typeable)

    
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Module n e t) where
  pretty (Module n p its) =
    PP.text "Module:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty (Text.unpack n)
        PP.<$>
        PP.text "path:" <+> PP.pretty (Text.unpack <$> p)
        PP.<$>
        PP.text "items:" <+> PP.pretty its
      )