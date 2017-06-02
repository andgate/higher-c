{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Item.Function where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text                        as Text
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

import qualified Language.Hawk.Syntax.Name            as N
import qualified Language.Hawk.Syntax.Expression      as E
import qualified Language.Hawk.Syntax.StatementBlock  as SB
import qualified Language.Hawk.Syntax.Type            as T


type Function
  = Fun N.Source E.Source T.Source

type Function
  = Fun N.Valid E.Valid T.Valid

type Function
  = Fun N.Typed E.Typed T.Typed


data Function n e t
  = Fun
    { funName   :: n
    , funParams :: [n]
    , funBody   :: Maybe (Either (SB.StmtBlk n e t) e)
    }
  deriving (Eq, Show, Ord, Data, Typeable)

      

instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Function n e t) where
  pretty (Fun name params body) =
    PP.text "Function Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "params:" <+> PP.pretty params
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )
  
  
instance (Binary n, Binary e, Binary t) => Binary (Function n e t) where
  get =
      Var <$> get <*> get <*> get
      
  put (Var name params body) =
      put name >> put params >> put body