module StlcExampleDef where

import Numeric.Natural (Natural)

data Ty = N
        | Arr Ty Ty

data Term = Con Natural
          | Add Term Term
          | Var Natural
          | Lam Ty Term
          | App Term Term

data Expr = ECon Natural
          | EAdd Expr Expr
          | EVar Natural
          | ELam Ty Expr
          | EApp Expr Expr

