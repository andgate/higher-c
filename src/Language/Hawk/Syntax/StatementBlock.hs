{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.StatementBlock where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text                        as Text
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

import qualified Language.Hawk.Syntax.Name            as N
import qualified Language.Hawk.Syntax.Expression      as E
import qualified Language.Hawk.Syntax.Type            as T


type Source
  = StmtBlk N.Source E.Source T.Source

type Valid
  = StmtBlk N.Valid E.Valid T.Valid

type Typed
  = StmtBlk N.Typed E.Typed T.Typed


data StmtBlk n e t
  = Variable 
    { varName  :: n
    , varBody  :: Either (StatementBlock n e t) e
    }
  deriving (Eq, Show, Ord, Data, Typeable)

      

instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (ExprDecl n t) where
  pretty (ExprDecl name opinf vars tipe) =
    PP.text "Expression Declaration:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "op info:" <+> PP.pretty opinf
        PP.<$>
        PP.text "vars:" <+> PP.pretty vars
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
      )
  
  
instance (Binary n, Binary t) => Binary (ExprDecl n t) where
  get =
      ExprDecl <$> get <*> get <*> get <*> get
      
  put (ExprDecl oi n vs t) =
      put n >> put oi >> put vs >> put t