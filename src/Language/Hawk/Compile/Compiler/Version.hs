module Language.Hawk.Compile.Compiler.Version (version) where

import qualified Data.Version as Version
import qualified Paths_hawk as This
import Language.Hawk.Compile.Package (Version(Version))


rawVersion :: [Int]
rawVersion =
  Version.versionBranch This.version
  

version :: Version
version =
  case rawVersion of
    major : minor : patch : _ ->
      Version major minor patch
      
    [major, minor] ->
      Version major minor 0
      
    [major] ->
      Version major 0 0
      
    [] ->
      error "Could not detect version of hkc you are using"