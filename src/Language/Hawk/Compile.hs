
{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , AllowAmbiguousTypes
  #-}
module Language.Hawk.Compile
        ( hkc
        , HkcConfig(..)
        , HkcProduct(..)
        ) where

import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control
import Data.Bag
import Data.Foldable

import Language.Hawk.Load
import Language.Hawk.Parse
import Language.Hawk.Parse.Lex
import Language.Hawk.Parse.Lex.Error
import Language.Hawk.Syntax

import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.State
import Language.Hawk.Compile.Options
import Text.PrettyPrint.Leijen.Text (pretty)


import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.Map.Strict       as Map
import qualified Language.Hawk.Parse   as P
import qualified Language.Hawk.TypeCheck as Tc
import qualified Language.Hawk.TypeCheck.Environment as TcEnv
import qualified Language.Hawk.NameCheck as Nc
import qualified Language.Hawk.NameCheck.Environment as NcEnv


hkc :: HkcConfig -> IO ()
hkc = runHkc compile

compile
  :: ( MonadState s m, HasHkcState s
     , MonadReader c m , HasHkcConfig c
     , MonadLog (WithSeverity msg) m, AsHkcMsg msg, AsLdMsg msg, AsPsMsg msg, AsNcMsg msg, AsTcMsg msg
     , MonadChronicle (Bag e) m
     , AsHkcErr e, AsLdErr e, AsPsErr e, AsLexErr e , AsNcErr e, AsTcErr e
     , MonadIO m, MonadBaseControl IO m
     )
  => m ()
compile = do
  condemn $ do
    load
    xs <- use hkcFileTexts
    docToks <- mapM lexer xs
    forM_ docToks $ \declsToks ->
        forM_ declsToks $ \declToks ->
            parse declToks

  -- Name Checking
  defs <- use hkcDefs
  let env = NcEnv.fromList . Map.keys $ defs
      es = concat . Map.elems $ defs
  condemn $ mapM_ (Nc.namecheck env) es

  liftIO $ print "names are okay"

  -- Type Checking
  r <- Tc.inferTop TcEnv.empty []
  liftIO $ print r

  liftIO $ print "types are okay"
    
  -- Code Generation
  --codegen
  
  return ()
