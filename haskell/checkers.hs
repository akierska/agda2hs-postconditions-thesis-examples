module Main where

import Stlc
import Numeric.Natural (Natural)

type Fuel = Int

-- ============================================================================
-- Maybe Operators 
-- ============================================================================

backtrack :: [Maybe Bool] -> Maybe Bool
backtrack ms
  | Just True `elem` ms = Just True        
  | Nothing   `elem` ms = Nothing         
  | otherwise             = Just False   

andM :: Maybe Bool -> Maybe Bool -> Maybe Bool
andM (Just False) _           = Just False
andM _            (Just False) = Just False
andM (Just True)  (Just True)  = Just True
andM _            _            = Nothing

anyM :: (a -> Maybe Bool) -> [a] -> Maybe Bool
anyM f = backtrack . map f

-- ============================================================================
-- Lookup
-- ============================================================================

lookupVar :: [Ty] -> Natural -> Maybe Ty
lookupVar ctx x = case drop (fromIntegral x) ctx of
    (t : _) -> Just t
    []      -> Nothing

checkLookup :: [Ty] -> Natural -> Ty -> Maybe Bool
checkLookup ctx x t = case lookupVar ctx x of
    Just t' -> Just (t == t')
    Nothing -> Just False

-- ============================================================================
-- enum
-- ============================================================================

enumTyping :: Fuel -> Fuel -> [Ty] -> Term -> [Ty]
enumTyping 0    _       _   _ = []          
enumTyping fuel topFuel ctx e = case e of
    Con _ -> [N]
    Add e1 e2 ->
        [ N | N `elem` enumTyping (fuel - 1) topFuel ctx e1
            , N `elem` enumTyping (fuel - 1) topFuel ctx e2 ]
    Var x -> case lookupVar ctx x of
        Just t  -> [t]
        Nothing -> []
    Lam t1 e' ->
        [ Arr t1 t2 | t2 <- enumTyping (fuel - 1) topFuel (t1 : ctx) e' ]
    App e1 e2 ->
        [ t2 | Arr d t2 <- enumTyping (fuel - 1) topFuel ctx e1
             , d `elem` enumTyping (fuel - 1) topFuel ctx e2 ]

-- ============================================================================
-- Type checking: does `e` have type `t` under `ctx`? (your original structure,
-- one list entry per typing rule, combined with `backtrack`).
-- ============================================================================

checkTyping :: Fuel -> Fuel -> [Ty] -> Term -> Ty -> Maybe Bool
checkTyping 0 _ ctx e t = backtrack       -- out of fuel: only non-recursive rules
    [ case (e, t) of
        (Con _, N) -> Just True
        _          -> Nothing
    , case (e, t) of
        (Var x, _) -> checkLookup ctx x t
        _          -> Nothing 
    ]
checkTyping fuel topFuel ctx e t = backtrack
    [ case (e, t) of
        (Con _, N) -> Just True
        _          -> Just False
    , case (e, t) of
        (Add e1 e2, N) ->
            checkTyping (fuel - 1) topFuel ctx e1 N
            `andM`
            checkTyping (fuel - 1) topFuel ctx e2 N
        _ -> Just False
    , case (e, t) of
        (Var x, _) ->
            checkLookup ctx x t
        _ -> Just False
    , case (e, t) of
        (Lam t1 e', Arr t1' t2) ->
            Just (t1 == t1')
            `andM`
            checkTyping (fuel - 1) topFuel (t1 : ctx) e' t2
        _ -> Just False
    , case (e, t) of
        (App e1 e2, t2) ->
            anyM (\t1 -> checkTyping (fuel - 1) topFuel ctx e1 (Arr t1 t2))
                 (enumTyping (fuel - 1) topFuel ctx e2)
        _ -> Just False
    ]

-- ============================================================================
-- Tests
-- ============================================================================

fuel :: Fuel
fuel = 100

wellTyped :: [([Ty], Term, Ty)]
wellTyped =
    [ ([],   Con 5,                       N)            
    , ([],   Add (Con 1) (Con 2),         N)           
    , ([N],  Var 0,                       N)          
    , ([],   Lam N (Var 0),               Arr N N)   
    , ([],   App (Lam N (Var 0)) (Con 3), N)        
    ]

illTyped :: [([Ty], Term, Ty)]
illTyped =
    [ ([],   Con 5,                       Arr N N)      
    , ([],   Add (Con 1) (Lam N (Var 0)), N)           
    , ([],   Var 0,                       N)          
    , ([],   App (Con 1) (Con 2),         N)         
    ]

test_illTypedFalse :: Bool
test_illTypedFalse =
    all (\(ctx, e, t) -> checkTyping fuel fuel ctx e t == Just False) illTyped

test_wellTypedTrue :: Bool
test_wellTypedTrue =
    all (\(ctx, e, t) -> checkTyping fuel fuel ctx e t == Just True) wellTyped

main :: IO ()
main = do
    report "ill-typed terms reject (Just False)" test_illTypedFalse
    report "well-typed terms accept (Just True)" test_wellTypedTrue
  where
    report name ok = putStrLn $ (if ok then "PASS  " else "FAIL  ") ++ name