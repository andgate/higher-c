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


typesig0 :: HkParsing m => m (Maybe Type.Source)
typesig0 =
  optional $ try typesig

typesig :: HkParsing m => m Type.Source
typesig =
  hasType >> typ2


-- Single Type
typ0 :: HkParsing m => m Type.Source
typ0 =
      (try tyPrim <?> "Primitive Type")
  <|> (try tyTuple <?> "Type Tuple")
  <|> (tyCon <?> "Type Constructor")


typ1 :: HkParsing m => m Type.Source
typ1 = 
  Type.apply <$> typ0 <*> many typ0


typ2 :: HkParsing m => m Type.Source
typ2 = withRegion arrArgs Type.arrow
  where
    arrArgs = rightArrowSep typ1



-- Primitive Type
tyPrim :: HkParsing m => m Type.Source
tyPrim =
  locate . try $ Type.Con <$> tyPrimName

  
tyTuple :: HkParsing m => m Type.Source
tyTuple = withRegion tupleArgs Type.tuple
  where
    tupleArgs = parens (commaSep typ1)
    

tyCon :: HkParsing m => m Type.Source
tyCon = locate $ Type.Con <$> conName


tyPrimName :: HkParsing m => m Name.Source
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