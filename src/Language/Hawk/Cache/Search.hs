module Language.Hawk.Cache.Search where

import Data.Text (Text)

import qualified Data.Text                 as T
import qualified Language.Hawk.Cache.Model as Db
import qualified Language.Hawk.Cache.Types as Db

search :: Text -> [Db.ItemId]
search mp = undefined
  {--
  Searching for items in a module path should be quick and simple.
   First search for a module where the qname == mp.
   
   first, is it an item or a module?
   First, look up the path as an item. Is there a hit? Return that.
   Otherwise, look up the module path.
     When a module is found, traverse the closure table,
     and get every submodule for that module.
     Then, with the list of modules, query the item table, and grab
     the ids of all the items that are children of those modules.
     Return that list.
   If no item or module is found, report the error.

   if that comes up, then it can be treated as a module path.
     use the closure table to find all the descendent modules of that module.
     then select all the items with the list of relevent module ids.
  --}