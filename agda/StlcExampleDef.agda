module StlcExampleDef where

open import Haskell.Prelude

data Ty : Type where
    N   : Ty
    Arr : Ty → Ty → Ty

data Term : Type where
    Con : Nat → Term
    Add : Term → Term → Term
    Var : Nat → Term
    Lam : Ty → Term → Term
    App : Term → Term → Term

{-# COMPILE AGDA2HS Ty #-}
{-# COMPILE AGDA2HS Term #-}

data Lookup : List Ty → Nat → Ty → Type where
    Here  : ∀ {Γ t}
          → Lookup (t ∷ Γ) 0 t

    There : ∀ {Γ t s n}
          → Lookup Γ n t
          → Lookup (s ∷ Γ) (suc n) t

data Typing : List Ty → Term → Ty → Type where
    TCon : ∀ {Γ n}
         → Typing Γ (Con n) N

    TAdd : ∀ {Γ e₁ e₂}
         → Typing Γ e₁ N
         → Typing Γ e₂ N
         → Typing Γ (Add e₁ e₂) N

    TVar : ∀ {Γ x t}
         → Lookup Γ x t
         → Typing Γ (Var x) t

    TLam : ∀ {Γ e t₁ t₂}
         → Typing (t₁ ∷ Γ) e t₂
         → Typing Γ (Lam t₁ e) (Arr t₁ t₂)

    TApp : ∀ {Γ e₁ e₂ t₁ t₂}
         → Typing Γ e₁ (Arr t₁ t₂)
         → Typing Γ e₂ t₁
         → Typing Γ (App e₁ e₂) t₂

data Expr (@0 Γ : List Ty) : @0 Ty → Type where
    ECon : Nat → Expr Γ N

    EAdd : Expr Γ N → Expr Γ N → Expr Γ N

    EVar : ∀ {@0 t}
         → (x : Nat)
         → @0 Lookup Γ x t
         → Expr Γ t

    ELam : ∀ {@0 t₂}
         → (t₁ : Ty)
         → Expr (t₁ ∷ Γ) t₂
         → Expr Γ (Arr t₁ t₂)

    EApp : ∀ {@0 t₁ t₂}
         → Expr Γ (Arr t₁ t₂)
         → Expr Γ t₁
         → Expr Γ t₂

{-# COMPILE AGDA2HS Expr #-}