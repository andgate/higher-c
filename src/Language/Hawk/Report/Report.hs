{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Report where

import Data.Aeson ((.=))
import qualified Data.Aeson.Types as Json
import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen
    ( Doc, SimpleDoc(..), (<>), displayS, displayIO, dullcyan, fillSep
    , hardline, renderPretty, text
    )
    

import qualified Language.Hawk.Report.Region as R


data Report
  = Report
    { _title      :: String
    , _highlight  :: Maybe R.Region
    , _preHint    :: Doc
    , _postHint   :: Doc
    }


report :: String -> Maybe R.Region -> String -> Doc -> Report
report title highlight pre post =
  Report title highlight (fillSep (text <$> words pre)) post
  
  
toJson :: [Json.Pair] -> Report -> (Maybe R.Region, [Json.Pair])
toJson extraFields (Report title subregion pre post) =
  let 
    fields =
      [ "tag"       .= title
      , "overview"  .= nonAnsiRender pre
      , "details"   .= nonAnsiRender post
      ]
  in
    (subregion, fields ++ extraFields)



toDoc :: String -> R.Region -> Report -> String -> Doc
toDoc location region (Report title highlight preHint postHint) source =
  messageBar title location
  <> hardline <> hardline <>
  preHint
  -- <> hardline <> hardline <>
  -- Code.render highlight region source
  <> hardline <>
  postHint
  <> hardline <> hardline
  
  
messageBar :: String -> String -> Doc
messageBar tag location =
  let
    usedSpace =
      4 + length tag + 1 + length location
  in 
    dullcyan $ text $
      "-- " ++ tag ++ " " ++ replicate (max 1 $ 80 - usedSpace) '-' ++ " " ++ location
      
      
toHandle :: Handle -> String -> R.Region -> Report -> String -> IO ()
toHandle handle location region rprt source =
  displayIO
    handle
    (renderPretty 1 80 $ toDoc location region rprt source)
   
   
toString :: String -> R.Region -> Report -> String -> String
toString location region rprt source =
  nonAnsiRender (toDoc location region rprt source)
  

nonAnsiRender :: Doc -> String
nonAnsiRender doc =
  displayS (stripAnsi $ renderPretty 1 80 doc) ""
  

stripAnsi :: SimpleDoc -> SimpleDoc
stripAnsi simpleDoc =
  case simpleDoc of
    SFail ->
      SFail
      
    SEmpty ->
      SEmpty
      
    SChar chr subDoc ->
      SChar chr (stripAnsi subDoc)
      
    SText n str subDoc ->
      SText n str (stripAnsi subDoc)
      
    SLine n subDoc ->
      SLine n (stripAnsi subDoc)
      
    SSGR _ subDoc ->
      stripAnsi subDoc