module Language.Hawk.Compile ( compile
                             , CompilerState (..)
                             , CompilerPhase (..)
                             , Package (..)
                             )
                             where

import Pipes
import qualified Pipes.Prelude as P
import Control.Monad ( forM_ )
import Control.Monad.IO.Class (liftIO)
import Language.Hawk.Compile.Monad
import Language.Hawk.Metadata
--import Language.Hawk.TypeCheck (typecheck)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ( (</>) )

import qualified Control.Monad.Trans.State.Strict as St
import qualified Language.Hawk.Parse              as P
import qualified Language.Hawk.Syntax.Item        as I
import qualified Language.Hawk.Syntax.Expression  as E
import qualified Language.Hawk.Syntax.Module      as M


compile
  :: CompilerState
  -> IO ()
compile s =
  runCompiler s $
    do
      loadPackages
      --typecheck
      --optimize
      --codegen

-- | Parse the modules in the given packages and store them accordingly
loadPackages :: Compiler ()
loadPackages = do
    s <- St.get
    -- Insert all the given packages
    mapM_ (liftIO . loadPackage) $ cPkgs s

loadPackage :: Package -> IO ()
loadPackage (Package n d) = undefined
  -- Pipes
  -- dirCrawl n d >-> parser >-> moduleCache

  {-
    liftIO $ runSqlite "hk.db" $ do
    runMigration Db.migrateAll

    pkgId <- insertPackage pkg
    forM_ (pkgSrcPaths pkg) $ \srcPath -> do
        src <- liftIO $ Text.readFile x
        m <- liftIO $ P.parseTopLevel src
        modId <- insertModule pkgId m src

        -- For testing only
        P.parseTest src
    -}



getRecursiveContents :: FilePath -> Producer FilePath IO ()
getRecursiveContents topPath = do
  names <- lift $ getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else yield path