module Stlc where

open import Haskell.Prelude
open import Haskell.Extra.Refinement

-- ==================================================================================
-- STLC EXTRINSIC AND INTRINSIC DEF 
-- ==================================================================================

data Ty : Type where
    N   : Ty
    Arr : Ty → Ty → Ty

data Term : Type where
    Con : Nat → Term
    Add : Term → Term → Term
    Var : Nat → Term
    Lam : Ty → Term → Term
    App : Term → Term → Term

{-# COMPILE AGDA2HS Ty deriving (Eq, Show) #-}
{-# COMPILE AGDA2HS Term deriving (Eq, Show) #-}

data Lookup : List Ty → Nat → Ty → Type where
    Here  : ∀ {Γ t}
          → Lookup (t ∷ Γ) zero t
    There : ∀ {Γ x t s}
          → Lookup Γ x t
          → Lookup (s ∷ Γ) (suc x) t

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

-- ==================================================================================
-- TRANSFORM: LEMMA AND SIGMA 
-- ==================================================================================

transform : Term → Term
transform (Con n)     = Con n
transform (Add e₁ e₂) = Add (transform e₂) (transform e₁)
transform (Var x)     = Var x
transform (Lam t e)   = Lam t (transform e)
transform (App e₁ e₂) = App (transform e₁) (transform e₂)

{-# COMPILE AGDA2HS transform #-}
postulate 
     preservesTyping : (Γ : List Ty)
                → (e : Term)
                → (t : Ty)
                → Typing Γ e t
                → Typing Γ (transform e) t

transformSigma : (@0 Γ : List Ty) → (e : Term)
    → (@0 t : Ty) → @0 Typing Γ e t
    → ∃ Term (λ e' → Typing Γ e' t)
transformSigma Γ e t d = transform e ⟨ preservesTyping Γ e t d ⟩

{-# COMPILE AGDA2HS transformSigma #-}


-- decTyping : (Γ : List Ty) → (e : Term) → (t : Ty)
--         → Dec (Typing Γ e t)

-- decTyping Γ (Con n)     N = True ⟨ TCon ⟩
-- decTyping Γ (Add e₁ e₂) N =
--   case decTyping Γ e₁ N , decTyping Γ e₂ N of λ where
--     (True ⟨ p₁ ⟩ , True ⟨ p₂ ⟩) →
--       True ⟨ TAdd p₁ p₂ ⟩
--     (False ⟨ ¬p ⟩ , _) →
--       False ⟨ ... ⟩
--     (_ , False ⟨ ¬p ⟩) →
--       False ⟨ ... ⟩
-- ... -- remaining cases