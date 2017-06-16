{-# LANGUAGE TypeFamilies
           , GADTs
           , DataKinds
           , ConstraintKinds
           , EmptyCase
           , StandaloneDeriving
           , TypeOperators
           , PatternSynonyms
           , FlexibleInstances
           , FlexibleContexts
           , OverloadedStrings
           , UndecidableInstances
  #-}
module Language.Hawk.Syntax.HkLit where

import Data.Binary
import GHC.Types (Constraint)
import Language.Hawk.Syntax.Pass
import Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

-- -----------------------------------------------------------------------------
-- | Type

data HkLit x
  = HkInt (XHkInt x) Integer
  | HkDouble (XHkFloat x) Double
  | HkChar (XHkChar x) Char
  | HkString (XHkString x) String
  | HkBoolean (XHkBoolean x) Bool
  | HkLit (XHkLit x)

type family XHkInt x
type family XHkFloat x
type family XHkChar x
type family XHkString x
type family XHkBoolean x
type family XHkLit x

type ForallHkLit (c :: * -> Constraint) (x :: *) =
  ( c (XHkInt x)
  , c (XHkFloat x)
  , c (XHkChar x)
  , c (XHkString x)
  , c (XHkBoolean x)
  , c (XHkLit x)
  )


deriving instance ForallHkLit Show x => Show (HkLit x)
deriving instance ForallHkLit Eq x => Eq (HkLit x)
deriving instance ForallHkLit Ord x => Ord (HkLit x)

data UD

type instance XHkInt      UD = ()
type instance XHkFloat    UD = ()
type instance XHkChar     UD = ()
type instance XHkString   UD = ()
type instance XHkBoolean  UD = ()
type instance XHkLit      UD = ()

type instance XHkInt      HkcPs = ()
type instance XHkFloat    HkcPs = ()
type instance XHkChar     HkcPs = ()
type instance XHkString   HkcPs = ()
type instance XHkBoolean  HkcPs = ()
type instance XHkLit      HkcPs = ()


type instance XHkInt      HkcRn = ()
type instance XHkFloat    HkcRn = ()
type instance XHkChar     HkcRn = ()
type instance XHkString   HkcRn = ()
type instance XHkBoolean  HkcRn = ()
type instance XHkLit      HkcRn = ()


type instance XHkInt      HkcTc = ()
type instance XHkFloat    HkcTc = ()
type instance XHkChar     HkcTc = ()
type instance XHkString   HkcTc = ()
type instance XHkBoolean  HkcTc = ()
type instance XHkLit      HkcTc = ()

-- -----------------------------------------------------------------------------
-- | Instances

-- Pretty ---------------------------------------------------------------------
instance ForallHkLit PP.Pretty x => PP.Pretty (HkLit x) where
  pretty lit =
    case lit of
      HkInt x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
         
      HkDouble x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
      
      HkChar x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
      
      HkString x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
      
      HkBoolean x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x

      HkLit x ->
        PP.string "Lit Con Ext:" <+> PP.pretty x


-- Binary ---------------------------------------------------------------------
instance ForallHkLit Binary x => Binary (HkLit x) where
  get = do
    n <- getWord8
    case n of
      1 -> HkInt <$> get <*> get
      2 -> HkDouble <$> get <*> get
      3 -> HkChar <$> get <*> get
      4 -> HkString <$> get <*> get
      5 -> HkBoolean <$> get <*> get
      6 -> HkLit <$> get
      _ -> error "unexpected input"

  put literal =
    case literal of
      HkInt x v     -> putWord8 1 >> put x >> put v
      HkDouble x v  -> putWord8 2 >> put x >> put v
      HkChar x v    -> putWord8 3 >> put x >> put v
      HkString x v  -> putWord8 4 >> put x >> put v
      HkBoolean x v -> putWord8 5 >> put x >> put v
      HkLit x       -> putWord8 6 >> put x