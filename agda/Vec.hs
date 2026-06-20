module Vec where

data Vec a = Nil
           | Cons a (Vec a)

concatV :: Vec a -> Vec a -> Vec a
concatV Nil ys = ys
concatV (Cons x xs) ys = Cons x (concatV xs ys)

