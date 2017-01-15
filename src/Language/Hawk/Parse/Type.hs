module Language.Hawk.Parse.Type where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


typesig0 :: HkParser (Maybe Type.Source)
typesig0 =
  optional $ try typesig

typesig :: HkParser Type.Source
typesig =
  hasType >> typ2


-- Single Type
typ0 :: HkParser Type.Source
typ0 =
      (tyPrim <?> "Primitive Type")
  <|> (tyTuple <?> "Type Tuple")
  <|> (tyCon <?> "Type Constructor")


typ1 :: HkParser Type.Source
typ1 = 
  Type.apply <$> typ0 <*> many typ0


typ2 :: HkParser Type.Source
typ2 = withRegion arrArgs Type.arrow
  where
    arrArgs = rightArrowSep typ1



-- Primitive Type
tyPrim :: HkParser Type.Source
tyPrim =
  locate . try $ Type.Con <$> tyPrimName

  
tyTuple :: HkParser Type.Source
tyTuple = withRegion tupleArgs Type.tuple
  where
    tupleArgs = parens (commaSep typ1)
    

tyCon :: HkParser Type.Source
tyCon = locate $ Type.Con <$> conName


tyPrimName :: HkParser Name.Source
tyPrimName =
      symbol "I1"
  <|> symbol "I8"
  <|> symbol "I16"
  <|> symbol "I32"
  <|> symbol "I64"
  <|> symbol "I128"
  <|> symbol "F16"
  <|> symbol "F32"
  <|> symbol "F64"
  <|> symbol "F128" 