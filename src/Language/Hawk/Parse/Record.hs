module Language.Hawk.Parse.Record where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type

import qualified Language.Hawk.Syntax.Record as Rec
import qualified Language.Hawk.Syntax.Name as Name


record :: HkParser Rec.Source
record = locate $
  Rec.Record <$> conName <*> (condefsym >> record_fields)
    
    
record_fields :: HkParser [Rec.RecordField Name.Source]
record_fields =
  list (lineFold record_field <?> "Record Field")


record_field :: HkParser (Rec.RecordField Name.Source)
record_field = locate $
    Rec.RecordField <$> varName <*> typesig