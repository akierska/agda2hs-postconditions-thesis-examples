module Leq where 

open import Haskell.Prelude

data Leq : Nat → Nat → Type where
  LeqZero : {n : Nat}   → Leq 0 n
  LeqSuc  : {m n : Nat} → Leq m n
                        → Leq (suc m) (suc n)

leq24 : Leq 2 4
leq24 = LeqSuc (LeqSuc LeqZero)

