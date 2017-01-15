module Language.Hawk.Parse.MetaItem where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Language.Hawk.Parse.Alias
import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import Language.Hawk.Parse.Function
import Language.Hawk.Parse.Variable
import Language.Hawk.Parse.ModuleName
import Language.Hawk.Parse.Record
import Language.Hawk.Parse.Statement
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Statement as Stmt
import qualified Language.Hawk.Syntax.MetaItem as MI
import qualified Language.Hawk.Syntax.Type as Ty

  
metaItems :: HkParser [MI.Source]
metaItems = 
  list metaItem


metaItem :: HkParser MI.Source
metaItem =
      (try impMItem <?> "Import")
  <|> (try aliasMItem <?> "Type Alias")
  <|> (try recMItem <?> "Record")
  <|> (try fnMItem <?> "Function Metadata")
  <|> (varMItem <?> "Variable Metadata")


impMItem :: HkParser MI.Source
impMItem = locate $
  rightArrow >> (MI.Import <$> moduleNameRaw)  


aliasMItem :: HkParser MI.Source
aliasMItem = locate $
  MI.Alias <$> alias


recMItem :: HkParser MI.Source
recMItem = locate $
  MI.Record <$> record
 

fnMItem :: HkParser MI.Source
fnMItem =locate $ 
  MI.Function <$> functionInfo <* fndefsym <* stmtblock
  
varMItem :: HkParser MI.Source
varMItem = locate $
  MI.Variable <$> varInfo <* vardefsym <* expr