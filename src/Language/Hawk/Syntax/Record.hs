module Language.Hawk.Syntax.Record where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

type Source =
  Record Name.Source
  
type Valid =
  Record Name.Valid
  
type Canonical =
  Record Name.Canonical
  
type Typed =
  Record Name.Typed
  

data Record n
  = Record
    { name :: n
    , fields :: [RecordField n]
    }
  deriving (Eq, Show, Data, Typeable)


data RecordField n
  = RecordField n (Type.Type n)
  deriving (Eq, Show, Data, Typeable)
  
  
instance (PP.Pretty n) => PP.Pretty (Record n) where
    pretty (Record n fs) =
      PP.text "Record:"
      PP.<$>
      PP.indent 2
        ( PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "Fields:" <+> PP.pretty fs
        )

instance (PP.Pretty n) => PP.Pretty (RecordField n) where
    pretty (RecordField n t) =
      PP.text "RecordField:"
      PP.<$>
      PP.indent 2
        ( PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "type:" <+> PP.pretty t
        )
        
  
instance (Binary n) => Binary (Record n) where
  get =
    Record <$> get <*> get

  put (Record n rfs) =
    put n >> put rfs
          
          
instance (Binary n) => Binary (RecordField n) where
  get =
    RecordField <$> get <*> get

  put (RecordField n t) =
    put n >> put t