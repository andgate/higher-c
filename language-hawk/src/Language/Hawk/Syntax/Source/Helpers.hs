module Language.Hawk.Syntax.Source.Helpers where

import Language.Hawk.Syntax.Source



explicitLam :: Term -> Term -> Term
explicitLam = plicitLam Explicit

plicitLam :: Plicity -> Type -> Type -> Type
plicitLam p arg ret = TLam p (PAnno PWild arg) ret



constraintPi :: Type -> Type -> Type
constraintPi = plicitPi Constraint

implicitPi :: Type -> Type -> Type
implicitPi = plicitPi Implicit

explicitPi :: Type -> Type -> Type
explicitPi = plicitPi Explicit

plicitPi :: Plicity -> Type -> Type -> Type
plicitPi p arg ret = TPi p (PAnno PWild arg) ret




constraintLPi :: Type -> Type -> Type
constraintLPi = plicitLPi Constraint

implicitLPi :: Type -> Type -> Type
implicitLPi = plicitLPi Implicit

explicitLPi :: Type -> Type -> Type
explicitLPi = plicitLPi Explicit

plicitLPi :: Plicity -> Type -> Type -> Type
plicitLPi p arg ret = TLPi p (PAnno PWild arg) ret