module StlcPreservesLemma where

open import Haskell.Prelude
open import StlcDef

transform : Term → Term
transform t = t

preservesTyping : (Γ : List Ty)
                → (e : Term)
                → (t : Ty)
                → Typing Γ e t
                → Typing Γ (transform e) t

preservesTyping _ _ _ t = t

