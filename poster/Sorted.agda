open import Haskell.Prelude
open import Agda.Builtin.Equality
open import Haskell.Extra.Refinement
open import Haskell.Extra.Dec

insert : Nat → List Nat → List Nat
insert x []       = x ∷ []
insert x (y ∷ ys) = if x <= y then x ∷ y ∷ ys else y ∷ insert x ys

{-# COMPILE AGDA2HS insert #-}

sort : List Nat → List Nat
sort []       = []
sort (x ∷ xs) = insert x (sort xs)

{-# COMPILE AGDA2HS sort #-}

data Sorted : List Nat → Set where
  Nil  : Sorted []
  One  : ∀ {x} → Sorted (x ∷ [])
  Cons : ∀ {x y xs} → IsTrue (x <= y)
              → Sorted (y ∷ xs) → Sorted (x ∷ y ∷ xs)

x : Sorted (1 ∷ 2 ∷ [])
x = Cons IsTrue.itsTrue One

postulate 
    sortedLemma : ∀ xs → Sorted (sort xs)
    sortedSigma : ∀ xs → ∃ (List Nat) (λ ys → Sorted ys)

-- prop_sortedLemma xs = isSorted (sort xs)

data Vec (a : Type) : (@0 n : Nat) → Type where
  Nil : Vec a 0
  Cons : {@0 n : Nat} → a → Vec a n → Vec a (suc n)

{-# COMPILE AGDA2HS Vec #-}

tailV : {a : Type} {@0 n : Nat} → Vec a (suc n) → Vec a n
tailV (Cons x xs) = xs

{-# COMPILE AGDA2HS tailV #-}


instance
  decSorted : {xs : List Nat} → Dec (Sorted xs)
  decSorted {[]} = True ⟨ Nil ⟩
  decSorted {x ∷ []} = True ⟨ One ⟩
  decSorted {x ∷ y ∷ xs} = mapDec
    (λ where (p , q) → Cons p q)
    (λ where (Cons p q) → p , q)
    iDecPair
