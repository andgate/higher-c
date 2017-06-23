{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , InstanceSigs
  #-}
module Language.Hawk.Cache.Item where

import Control.Lens
import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Binary
import Data.Text (Text, pack, intercalate)
import Database.Persist
import Language.Hawk.Cache.Model (BackendT)
import Language.Hawk.Report.Result
import Language.Hawk.DepExpansion
import Language.Hawk.Syntax (funName, nameText)

import qualified Data.ByteString.Lazy as BS
import qualified Language.Hawk.Cache.Model as Db
import qualified Language.Hawk.Cache.Types as Db
import qualified Language.Hawk.Syntax as Syn

class Cacheable a where
    cache :: MonadIO m => a -> BackendT m (Result ())


instance (Syn.BinaryX x) => Cacheable ( Db.PackageId
                                  , Db.ModuleId
                                  , Db.ModuleFileId
                                  , Syn.Item x
                                  ) where
    cache (pid, mid, mfid, i) = do
      iid <- insert $ Db.Item pid mid mfid Db.Fresh (BS.toStrict $ encode i)

      case i of
        Syn.DepItem dep -> void $ cache (iid, dep)
        Syn.ForeignItem _ -> insert_ $ Db.Foreign iid
        Syn.ExposeItem _ -> insert_ $ Db.Expose iid

        Syn.VowItem _ -> insert_ $ Db.Vow iid
        Syn.SigItem _ -> insert_ $ Db.TypeSig iid

        Syn.VarItem _ -> insert_ $ Db.Var iid
        Syn.ValItem _ -> insert_ $ Db.Val iid
        Syn.FunItem f -> do
          -- This needs to store a qualified name as well
          insert_ $ Db.Fun iid
          insert_ $ Db.VarSymbol iid (f^.funName.nameText) (f^.funName.nameText) 
        
        -- TODO: These require a check for conflicts with module names
        Syn.NewTyItem _ -> insert_ $ Db.NewType iid
        Syn.TyAliasItem _ -> insert_ $ Db.TypeAlias iid
        
        Syn.TyClassItem _ -> insert_ $ Db.TypeClass iid
        Syn.TyInstItem _ -> insert_ $ Db.TypeClassInst iid

        Syn.DataItem _ -> insert_ $ Db.DataType iid

      return $ return ()


instance Cacheable (Db.ItemId, Syn.Dependency) where
    cache :: MonadIO m => (Db.ItemId, Syn.Dependency) -> BackendT m (Result ())
    cache (iid, dep) = do
        forM_ (expandDep dep) $ \(EDep ps ex q a) ->
            let p = intercalate "." ps
            in insert_ $ Db.ExpandedDependency iid p ex q a
        return $ return ()