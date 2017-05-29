-- Core.Emit is where desugaring Syntax AST into Core AST occurs.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Hawk.Core.Emit where
{-
import Data.List
import Safe as S

import Language.Hawk.Data.Emittable
import Language.Hawk.Data.Node

import qualified Language.Hawk.Core.AST as Core
import qualified Language.Hawk.Syntax.AST as Syn


-- -----------------------------------------------------------------------------
-- | Error reporting
notImpl :: HkNode n => n -> String -> a
notImpl n feature_name 
    = error $ prefixError n ++ ": " ++ feature_name ++ " is not an implemented language feature." 


-- -----------------------------------------------------------------------------
-- | Module
instance Emittable Syn.HkModNode Core.Mod where
    emit (Syn.HkMod path (Syn.HkModBlock items _) _)
        = Core.Mod path' items'
        where path' = show path
              items' = map emit items


-- -----------------------------------------------------------------------------
-- | Module Items
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
        
    emit (Syn.HkModItem vis (Syn.HkItemFn (Syn.HkFnDef name (Just qty) match _)) a)
        = Core.ItemFn fn_decl expr
        where fn_decl = Core.FnDecl vis' name' retty params 
              vis' = emit vis
              name' = show name
              retty = mkRetty qty
              params = mkParams qty
              expr = emit match
        
    emit (Syn.HkModItem vis (Syn.HkItemBind _) a)
        = notImpl a "Bind"
    
    emit (Syn.HkModItem _ (Syn.HkItemType _) a)
        = notImpl a "Type Declaration"


-- -----------------------------------------------------------------------------
-- | Visibility       
instance Emittable Syn.HkVisibilityTagNode Core.Visibility where
    emit (Syn.HkPublic _) = Core.Public
    emit (Syn.HkPrivate _) = Core.Private
  
  
  
-- -----------------------------------------------------------------------------
-- | Multi-Match

data CasePath
  = CaseBranch Core.ArmCon [CasePath]
  | CaseResult Core.Expr

cpath_to_case :: [CasePath] -> Core.Expr
cpath_to_case ps = cpath_to_case' 1 ps

cpath_to_case' :: Int -> [CasePath] -> Core.Expr
cpath_to_case' _ [CaseResult e] = e
cpath_to_case' _ [CaseBranch _ []] = error "Malformed CasePath: CaseBranch was found empty."

cpath_to_case' i ps =
  Core.Case xn ars
  where xn = Core.Var $ "x" ++ show i
        ars = [Core.Arm ar (cpath_to_case' (i+1) ps') | (CaseBranch ar ps') <- ps]


pats_to_cpath :: [CasePath] -> ([Core.ArmCon], Core.Expr) -> [CasePath]
pats_to_cpath [] ([], r) = [CaseResult r]
pats_to_cpath [] ((a:as), r) = [CaseBranch a (pats_to_cpath [] (as, r))]
pats_to_cpath (p@(CaseBranch a2 ps'):ps) t@((a1:as), r)
  = if a1 == a2
       then (CaseBranch a1 (ps' ++ next)) : ps
       else p : (pats_to_cpath ps t)
  where next = pats_to_cpath ps' (as, r)
pats_to_cpath _ _ = [] -- This should never be reached


is_cbranch :: Core.ArmCon -> CasePath -> Bool
is_cbranch a1 (CaseBranch a2 _) = a1 == a2
is_cbranch _ _ = False

instance Emittable Syn.HkMultiMatchNode Core.Expr where
  emit (Syn.HkMultiMatch arms a)
    = error $ show case_expr
    where rs = map (emit . Syn.mm_arm_rhs) arms
          pss = map (map emit . Syn.mm_arm_pats) $ arms
          arms' = zip pss rs
          cpaths = foldl' pats_to_cpath [] arms'
          case_expr = cpath_to_case cpaths
              
-- -----------------------------------------------------------------------------
-- | Pattern       
instance Emittable Syn.HkPatternNode Core.ArmCon where
  emit (Syn.HkPatIdent mut name a)
    = Core.VarArm $ show name
  
  emit (Syn.HkPatAlias mut name p a)
    = notImpl a "Pattern Alias"
      
  emit (Syn.HkPatConst c a)
    = Core.ConstArm $ emit c
  
  emit (Syn.HkPatRec qcon ps a)  
    = notImpl a "Record Constructor Pattern"
  
  emit (Syn.HkPatTuple ps a)
    = notImpl a "Tuple Pattern"
  
  emit (Syn.HkPatRef p mut a)
    = notImpl a "Pattern Reference"
  
  emit (Syn.HkPatAny _)
      = Core.DefaultArm

-- -----------------------------------------------------------------------------
-- | Pattern
instance Emittable Syn.HkMatchRhsNode Core.Expr where
    emit (Syn.HkMatchBlock bl _)
        = emit bl
    emit (Syn.HkMatchExp e _)
        = emit e
    emit (Syn.HkMatchGuardedRhs gs a)
        = notImpl a "Pattern Guards"

instance Emittable Syn.HkBlockNode Core.Expr where
    emit (Syn.HkBlock stmts a)
        = Core.Block $ map emit stmts
        
instance Emittable Syn.HkBlockStmtNode Core.Expr where
    emit (Syn.HkStmtBlock bl a)
        = emit bl
        
    emit (Syn.HkStmtExp e a)
        = emit e
  
    emit (Syn.HkStmtBind bind)
        = notImpl bind "Let Binding Statement"

    emit (Syn.HkStmtAssign lhs rhs a)
        = notImpl a "Assignment Statement"
  
    emit (Syn.HkStmtReturn e _)
        = Core.Return $ emit e
  
    emit (Syn.HkStmtMatch e ms a)
        = notImpl a "Match Statement"
  
    emit (Syn.HkStmtIf cond if_bl else_bl a)
        = notImpl a "If Statement"
    
    emit (Syn.HkStmtElse else_bl a)
        = notImpl a "Else Statement"
  
    emit (Syn.HkStmtWhile cond bl a)
        = notImpl a "While Statement"
        
    emit (Syn.HkStmtDoWhile cond bl a)
        = notImpl a "Do-While Statement"
  
    emit (Syn.HkStmtFor maybe_init maybe_cond maybe_expr bl a)
        = notImpl a "For Statement"
        
    emit (Syn.HkStmtForEach iter gen  bl  a)
        = notImpl a "For-Each Statement"
        
    emit (Syn.HkStmtForEachIx index iter gen bl a)
        = notImpl a "Indexed For-Each Statement"
  
    emit (Syn.HkStmtEmpty a)
        = notImpl a "Empty Statement"


instance Emittable Syn.HkExpNode Core.Expr where
    emit (Syn.HkConstExp c)
        = Core.Const $ emit c
    emit (Syn.HkExpVar n) = notImpl n "Variable Expression"
    emit (Syn.HkExpCon n) = notImpl n "Constructor Expression"
    
    emit (Syn.HkExpPrefixApp _ _ a)
        = notImpl a "Prefix Application Expression"
    emit (Syn.HkExpInfixApp _ _ _ a)
        = notImpl a "Infix Application Expression"
    
    emit (Syn.HkExpApp _ _ a)
        = notImpl a "Expression Application"
    emit (Syn.HkExpCast _ _ a)
        = notImpl a "Expression Typecast"
    
    emit (Syn.HkExpIfThenElse _ _ _ a)
        = notImpl a "If-Then-Else Expression"
    
    emit (Syn.HkExpLambda _ _ a)
        = notImpl a "Lambda Expession"
    
    emit (Syn.HkExpDo e _)
        = emit e
    

-- -----------------------------------------------------------------------------
-- | Constant

instance Emittable Syn.HkConstNode Core.Constant where
    emit (Syn.HkUnit _) = Core.ConstVoid
    emit (Syn.HkBool v _) = Core.ConstBool v
    
    emit (Syn.HkW8 v _) = Core.ConstW8 v
    emit (Syn.HkW16 v _) = Core.ConstW16 v
    emit (Syn.HkW32 v _) = Core.ConstW32 v
    emit (Syn.HkW64 v _) = Core.ConstW64 v
    
    emit (Syn.HkI8 v _) = Core.ConstI8 v
    emit (Syn.HkI16 v _) = Core.ConstI16 v
    emit (Syn.HkI32 v _) = Core.ConstI32 v
    emit (Syn.HkI64 v _) = Core.ConstI64 v
    
    emit (Syn.HkF32 v _) = Core.ConstF32 v
    emit (Syn.HkF64 v _) = Core.ConstF64 v
    
    emit (Syn.HkChar v _) = Core.ConstChar v
    emit (Syn.HkString v _) = Core.ConstString v

-- -----------------------------------------------------------------------------
-- | Type
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

-- -----------------------------------------------------------------------------
-- | Primitive Types
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


-- -----------------------------------------------------------------------------
-- | Helpers
mkRetty :: Syn.HkQualTypeNode -> Core.Type
mkRetty = emit . Syn.evalQType

mkParams :: Syn.HkQualTypeNode -> [Core.Param]
mkParams qty = zipWith Core.Param tys' xs
  where
    tys = Syn.qtypeArgs qty
    tys' = map emit tys
    xs = ["x" ++ show i | i <- [1..] ]

    -}