{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Syntax.Function where

import Data.Binary
import Data.Data
import Data.Typeable
import Database.Persist.TH
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy                   as Text
import qualified Text.PrettyPrint.ANSI.Leijen     as PP
import qualified Language.Hawk.Syntax.Binding     as B
import qualified Language.Hawk.Syntax.Expression  as Expr
import qualified Language.Hawk.Syntax.Name        as N
import qualified Language.Hawk.Syntax.Statement   as Stmt
import qualified Language.Hawk.Syntax.Type        as Type


type Source
  = Function N.Source Expr.Source (Maybe Type.Source)

type Valid
  = Function N.Valid Expr.Valid (Maybe Type.Valid)

type Typed
  = Function N.Typed Expr.Typed Type.Typed


data Function n e t
  = Function 
    { fn_op   :: OpInfo
    , fn_name :: n
    , fn_args :: [B.Binding n]
    , fn_type :: t
    , fn_body :: Stmt.Block n e t
    }
  deriving (Eq, Show, Ord, Data, Typeable)

 

data OpInfo
  = OpInfo
    { prec :: Integer
    , assoc :: Assoc
    }
  deriving (Eq, Show, Ord, Data, Typeable)
  
data Assoc = AssocL | AssocR | AssocN
  deriving (Eq, Show, Read, Ord, Data, Typeable)

derivePersistField "Assoc"


defOpInfo :: OpInfo
defOpInfo = OpInfo 5 AssocR

assocToString :: Assoc -> String
assocToString a =
  case a of
      AssocL -> "l"
      AssocR -> "r"
      AssocN -> "n"
      

assocFromName :: N.Name -> Assoc
assocFromName (N.Name t _) =
  assocFromString $ Text.unpack t
      
assocFromString :: String -> Assoc
assocFromString t =
  case t of
      "l" -> AssocL
      "r" -> AssocR
      "n" -> AssocN
      
      
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Function n e t) where
  pretty (Function name opinf args tipe body) =
    PP.text "Function:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "op info:" <+> PP.pretty opinf
        PP.<$>
        PP.text "args:" <+> PP.pretty args
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )
      
      
instance PP.Pretty OpInfo where
  pretty (OpInfo p a) =
    PP.text "OpInfo:"
    PP.<$>
    PP.indent 2
      ( PP.text "precedence:" <+> PP.pretty p
        PP.<$>
        PP.text "associativity:" <+> PP.pretty (assocToString a)
      )
  
  
instance (Binary n, Binary e, Binary t) => Binary (Function n e t) where
  get =
      Function <$> get <*> get <*> get <*> get <*> get
      
  put (Function n oi as t b) =
      put n >> put oi >> put as >> put t >> put b
      
      
instance Binary OpInfo where
  get =
      OpInfo <$> get <*> get
      
  put (OpInfo p a) =
      put p >> put a
      
      
instance Binary Assoc where
  get = do
      v <- getWord8
      case v of
          1 -> return AssocL
          2 -> return AssocR
          3 -> return AssocN
          _ -> error "Expected Assoc, unexpected binary value."
      
  put a =
      case a of
          AssocL -> putWord8 1
          AssocR -> putWord8 2
          AssocN -> putWord8 3
        