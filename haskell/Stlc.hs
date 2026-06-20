module Stlc where

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

transform :: Term -> Term
transform (Con n) = Con n
transform (Add e₁ e₂) = Add (transform e₂) (transform e₁)
transform (Var x) = Var x
transform (Lam t e) = Lam t (transform e)
transform (App e₁ e₂) = App (transform e₁) (transform e₂)

transformSigma :: Term -> Term
transformSigma e = transform e