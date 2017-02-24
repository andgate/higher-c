{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Syntax.OpInfo where

import Data.Binary
import Data.Data
import Data.Typeable
import Database.Persist.TH
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy                   as Text
import qualified Text.PrettyPrint.ANSI.Leijen     as PP
import qualified Language.Hawk.Syntax.Name        as N


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


instance PP.Pretty OpInfo where
  pretty (OpInfo p a) =
    PP.text "OpInfo:"
    PP.<$>
    PP.indent 2
      ( PP.text "precedence:" <+> PP.pretty p
        PP.<$>
        PP.text "associativity:" <+> PP.pretty (assocToString a)
      )
      
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