-- Core.Emit is where desugaring Syntax AST into Core AST occurs.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Hawk.Core.Emit where

import Language.Hawk.Data.Emittable
import Language.Hawk.Data.Node

import qualified Language.Hawk.Core.AST as Core
import qualified Language.Hawk.Syntax.AST as Syn

notImpl :: HkNode n => n -> String -> a
notImpl n feature_name 
    = error $ prefixError n ++ ": " ++ feature_name ++ " is not an implemented language feature." 

instance Emittable Syn.HkModNode Core.Mod where
    emit (Syn.HkMod path (Syn.HkModBlock items _) _)
        = Core.Mod path' items'
        where path' = show path
              items' = map emit items

instance Emittable Syn.HkModItemNode Core.Item where
    emit (Syn.HkModItem _ (Syn.HkItemMod _) a)
        = notImpl a "Sub-Module"
        
    emit (Syn.HkModItem _ (Syn.HkItemImport _ _) a)
        = notImpl a "Import"
        
    emit (Syn.HkModItem _ (Syn.HkItemImportQual _ _) a)
        = notImpl a "Qualified Import"
        
    emit (Syn.HkModItem vis (Syn.HkItemFnLink _ _) a)
        = notImpl a "Linked Function"
        
    emit (Syn.HkModItem vis (Syn.HkItemFn (Syn.HkFnDec _ _ _)) a)
        = notImpl a "Function Declaration"
    
    emit (Syn.HkModItem vis (Syn.HkItemFn (Syn.HkFnDef _ Nothing _ _)) a)
        = notImpl a "Type Inference"
        
    emit (Syn.HkModItem vis (Syn.HkItemFn (Syn.HkFnDef name (Just qty) matches _)) a)
        = Core.ItemFn fn_decl exprs
        where fn_decl = Core.FnDecl vis' name' retty params 
              vis' = emit vis
              name' = show name
              retty = mkRetty qty
              params = mkParams qty
              exprs = map emit matches
        
    emit (Syn.HkModItem vis (Syn.HkItemBind _) a)
        = notImpl a "Bind"
    
    emit (Syn.HkModItem _ (Syn.HkItemType _) a)
        = notImpl a "Type Declaration"
        
instance Emittable Syn.HkVisibilityTagNode Core.Visibility where
    emit (Syn.HkPublic _) = Core.Public
    emit (Syn.HkPrivate _) = Core.Private
  
  
instance Emittable Syn.HkMultiMatchNode Core.Expr where
    emit (Syn.HkMultiMatch pats rhs a)
        = notImpl a "Multi-match unsupported"

instance Emittable Syn.HkTypeNode Core.Type where
    emit (Syn.HkTyFun _ _ a)
        = notImpl a "Type Function"
        
    emit (Syn.HkTyApp _ _ a)
        = notImpl a "Type Application"
    
    emit (Syn.HkTyPrim primty _)
        = Core.PrimTy $ emit primty
        
    emit (Syn.HkTyCon _ a)
        = notImpl a "Type Constructor"
        
    emit (Syn.HkTyVar _ a)
        = notImpl a "Type Variable"
        
    emit (Syn.HkTyPtr _ a)
        = notImpl a "Pointer Type"
        
    emit (Syn.HkTyArray _ a)
        = notImpl a "Array Type"
    
    emit (Syn.HkTyTuple _ a)
        = notImpl a "Tuple Type"


instance Emittable Syn.HkPrimTypeNode Core.PrimType where
    emit (Syn.HkTyUnit _) = Core.UnitTy
    emit (Syn.HkTyBool _) = Core.BoolTy
    emit (Syn.HkTyW8 _)   = Core.W8Ty
    emit (Syn.HkTyW16 _)  = Core.W16Ty
    emit (Syn.HkTyW32 _)  = Core.W32Ty
    emit (Syn.HkTyW64 _)  = Core.W64Ty
    emit (Syn.HkTyI8 _)   = Core.I8Ty
    emit (Syn.HkTyI16 _)  = Core.I16Ty
    emit (Syn.HkTyI32 _)  = Core.I32Ty
    emit (Syn.HkTyI64 _)  = Core.I64Ty
    emit (Syn.HkTyF32 _)  = Core.F32Ty
    emit (Syn.HkTyF64 _)  = Core.F64Ty
    emit (Syn.HkTyChar _) = Core.CharTy
    emit (Syn.HkTyString _) = Core.StringTy

-- Helpers for making functions
mkRetty :: Syn.HkQualTypeNode -> Core.Type
mkRetty = emit . Syn.evalQType

mkParams :: Syn.HkQualTypeNode -> [Core.Param]
mkParams qty = zipWith Core.Param tys' xs
  where
    tys = Syn.qtypeArgs qty
    tys' = map emit tys
    xs = ["x" ++ show i | i <- [1..] ]