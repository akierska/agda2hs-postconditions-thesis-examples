module StlcLemmaProp where

import Test.QuickCheck
import StlcDef
import StlcCheckers

transform :: Term -> Term
transform t = t

prop_preservesTyping :: [Ty]
                     -> Term
                     -> Ty
                     -> Property
prop_preservesTyping ctx e t =
    checkTyping ctx e t
    ==> checkTyping ctx (transform e) t