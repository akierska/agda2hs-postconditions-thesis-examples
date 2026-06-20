module Vec where 

open import Haskell.Prelude

data Vec (a : Type) : (@0 n : Nat) → Type where
  Nil  : Vec a 0
  Cons : {@0 n : Nat} → a → Vec a n → Vec a (suc n)

concatV : {a : Type} {@0 n m : Nat} → Vec a n → Vec a m → Vec a (n + m) 
concatV Nil ys = ys
concatV (Cons x xs) ys = Cons x (concatV xs ys) 

{-# COMPILE AGDA2HS Vec #-}
{-# COMPILE AGDA2HS concatV #-}