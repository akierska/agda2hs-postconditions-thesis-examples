module StlcDef where

import Numeric.Natural (Natural)

data Ty = N
        | Arr Ty Ty
    deriving Eq

data Term = Con Natural
          | Add Term Term
          | Var Natural
          | Lam Ty Term
          | App Term Term
    deriving Eq

data Expr = ECon Natural
          | EAdd Expr Expr
          | EVar Natural
          | ELam Ty Expr
          | EApp Expr Expr
    deriving Eq

