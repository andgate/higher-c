module Language.Hawk.Syntax.DataDefinition where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDeclaration as TD
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source = 
  DataDef N.Source T.Source
  
type Valid = 
  DataDef N.Valid T.Valid

type Typed = 
  DataDef N.Typed T.Typed

 
data DataDef n t
    = DataDef
      { data_decl :: TD.TypeDecl n t
      , data_cons :: [DataCons n t]
      }
    deriving (Eq, Show, Data, Typeable)

type DataDefBody n t =
  [DataCons n t]
    
data DataCons n t =
  DataCons n t [DataRow n t]
  deriving (Eq, Show, Ord, Data, Typeable)
  
data DataRow n t
  = DataRow (Maybe n) t
  deriving (Eq, Show, Ord, Data, Typeable)

mkRecDef :: TD.TypeDecl n [t] -> [DataRow n [t]] -> DataDef n [t] 
mkRecDef td@(TD.TypeDecl _ n _ _) b = 
  DataDef td [DataCons n [] b]


instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (DataDef n t) where
    pretty (DataDef d b) =
      PP.text "Data Definition:"
      PP.<$>
      PP.indent 2
        ( PP.text "decl:" <+> PP.pretty d
          PP.<$>
          PP.text "body:" <+> PP.pretty b
        )


instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (DataCons n t) where
    pretty (DataCons n t b) =
      PP.text "Data Constructor:"
      PP.<$>
      PP.indent 2
        ( PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "Type:" <+> PP.pretty t
          PP.<$>
          PP.text "Body:" <+> PP.pretty b
        )
        
        
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (DataRow n t) where
    pretty (DataRow n t) =
      PP.text "Data Row:"
      PP.<$>
      PP.indent 2
        ( PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "Type:" <+> PP.pretty t
        )



instance (Binary n, Binary t) => Binary (DataDef n t) where
  get =
    DataDef <$> get <*> get

  put (DataDef d b) =
    put d >> put b
            

  
instance (Binary n, Binary t) => Binary (DataCons n t) where
  get =
    DataCons <$> get <*> get <*> get

  put (DataCons n t b) =
    put n >> put t >> put b
    
    
instance (Binary n, Binary t) => Binary (DataRow n t) where
  get =
    DataRow <$> get <*> get

  put (DataRow n t) =
    put n >> put t
              