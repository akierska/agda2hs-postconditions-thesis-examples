module Between where 

open import Haskell.Prelude
open import Leq

-- `Between k m j` is a proof that m lies between k and j, i.e. k ≤ m ≤ j.
-- m is an index so the type records *which* number is in between.
data Between : Nat → Nat → Nat → Type where
  mk : {k m j : Nat} → Leq k m → Leq m j
                               → Between k m j

-- Witness: 3 is between 2 and 4.
between-2-3-4 : Between 2 3 4
between-2-3-4 = mk (LeqSuc (LeqSuc LeqZero))
                   (LeqSuc (LeqSuc (LeqSuc LeqZero)))