{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Report where

import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen
    ( Doc, SimpleDoc(..), (<>), displayS, displayIO, dullcyan, fillSep
    , hardline, renderPretty, text, putDoc, vcat
    )
import qualified Language.Hawk.Report.Region as R


data Report
  = SimpleReport Doc
  | Report
    { _title      :: String
    , _highlight  :: Maybe R.Region
    , _preHint    :: Doc
    , _postHint   :: Doc
    }


class Reportable a where
    toReport :: a -> Report

class MultiReportable a where
    toReports :: a -> [Report]


report :: String -> Maybe R.Region -> String -> Doc -> Report
report title highlight pre =
  Report title highlight (fillSep (text <$> words pre))

simple :: String -> Report
simple = SimpleReport . text

putReport :: Report -> IO ()
putReport =
  putDoc . toDoc

putReports :: [Report] -> IO ()
putReports =
  putDoc . wrapHardLine . vcat . map toDoc

wrapHardLine :: Doc -> Doc
wrapHardLine d =
  hardline <> d <> hardline

toDoc :: Report -> Doc
toDoc (SimpleReport doc) =
  doc

toDoc (Report title highlight preHint postHint) =
  messageBar title ""
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