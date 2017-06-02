{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Item.Variable where

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


type Source
  = Var N.Source E.Source T.Source

type Valid
  = Var N.Valid E.Valid T.Valid

type Typed
  = Var N.Typed E.Typed T.Typed


data Variable n e t
  = Var 
    { varName  :: n
    , varBody  :: Maybe (Either (SB.StmtBlk n e t) e)
    }
  deriving (Eq, Show, Ord, Data, Typeable)

      

instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Variable n e t) where
  pretty (Var name body) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )
  
  
instance (Binary n, Binary e, Binary t) => Binary (Variable n e t) where
  get =
      Var <$> get <*> get
      
  put (Var name body) =
      put name >> put body