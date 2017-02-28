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
      , data_body :: DataDefBody n t
      }
    deriving (Eq, Show, Data, Typeable)

mkRecDef :: TD.TypeDecl n t -> DataConsBody n t -> DataDef n t
mkRecDef td@(TD.TypeDecl _ n _) b = 
  DataDef td [DataCons n b]
  

type DataDefBody n t =
  [DataCons n t]
    
data DataCons n t =
  DataCons n (DataConsBody n t)
  deriving (Eq, Show, Ord, Data, Typeable)
  
type DataConsBody n t =
  [DataMember n t]

data DataMember n t = 
   Tagless t
  |Tagged n t
  deriving (Eq, Show, Ord, Data, Typeable)
  
mkTagless :: t -> DataConsBody n t
mkTagless t = [Tagless t]
  

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
    pretty (DataCons n b) =
      PP.text "Data Constructor:"
      PP.<$>
      PP.indent 2
        ( PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "Body:" <+> PP.pretty b
        )
          

instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (DataMember n t) where
    pretty (Tagless t) =
      PP.text "Tagless Member:"
      PP.<$>
      PP.indent 2
        ( PP.text "Type:" <+> PP.pretty t
        )
        
    pretty (Tagged n t) =
      PP.text "Tagged Member:"
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
    DataCons <$> get <*> get

  put (DataCons n b) =
    put n >> put b
    
    

instance (Binary n, Binary t) => Binary (DataMember n t) where
  get = do
    n <- getWord8
    case n of
      1 -> Tagless <$> get
      2 -> Tagged <$> get <*> get
      _ -> error "Binary encounter unexpected input while serializing DataMember."
      
  put e =
    case e of
      Tagless t           -> putWord8 1 >> put t
      Tagged n t           -> putWord8 2 >> put n >> put t
              