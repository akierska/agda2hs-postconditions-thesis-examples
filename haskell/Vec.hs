module Vec where
import Test.QuickCheck

type Fuel = Int 

data Vec a = Nil
           | Cons a (Vec a)
  deriving Show

concatV :: Vec a -> Vec a -> Vec a
concatV Nil ys = ys
concatV (Cons x xs) ys = Cons x (concatV xs ys)

forAllJust :: (Show a, Testable prop) => Gen (Maybe a) -> (a -> prop) -> Property
forAllJust gen f = forAll gen $ \mx -> maybe discard f mx

checkVec :: Fuel -> Fuel -> Int -> Vec Int -> Maybe Bool
checkVec n v = undefined

genVec :: Fuel -> Fuel -> Gen (Maybe (Vec Int))
genVec fuel n = undefined

prop_concatV :: Int -> Int -> Int -> Property
prop_concatV fuel n m =
  forAllJust (genVec fuel n) $ \xs ->
  forAllJust (genVec fuel m) $ \ys ->
  checkVec fuel fuel (n + m) (concatV xs ys)
