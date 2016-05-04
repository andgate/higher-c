-- Core.Emit is where desugaring Syntax AST into Core AST occurs.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Hawk.Core.Emit where

import Language.Hawk.Data.Emittable

import qualified Language.Hawk.Core.AST as Core
import qualified Language.Hawk.Syntax.AST as Syn

instance Emittable Syn.HkTranslUnitNode Core.Mod where
    emit _ = undefined
