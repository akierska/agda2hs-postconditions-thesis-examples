module StlcCheckers where

import Numeric.Natural (Natural)
import StlcDef

-- ============================================================================
-- Shared helper
-- ============================================================================

checkLookup :: [Ty] -> Natural -> Ty -> Bool
checkLookup (t:_)  0 t' = t == t'
checkLookup (_:xs) n t  =
    n > 0 && checkLookup xs (n - 1) t
checkLookup []     _ _  = False

-- ============================================================================
-- PART 1: Hand-written checker (App not handled)
-- ============================================================================

checkTyping :: [Ty] -> Term -> Ty -> Bool
checkTyping _ (Con _) N = True
checkTyping ctx (Add e1 e2) N =
    checkTyping ctx e1 N
    && checkTyping ctx e2 N
checkTyping ctx (Var x) t =
    checkLookup ctx x t
checkTyping ctx (Lam t1 e) (Arr t1' t2) =
    t1 == t1'
    && checkTyping (t1 : ctx) e t2
checkTyping _ _ _ = False

-- ============================================================================
-- PART 2: Derived checker (simplified, without fuel)
-- ============================================================================

enumTyping :: [Ty] -> Term -> [Ty]
enumTyping = undefined

checkTypingD :: [Ty] -> Term -> Ty -> Bool
checkTypingD ctx e t = or
    [ case (e, t) of
        (Con _, N) -> True
        _          -> False
    , case (e, t) of
        (Add e1 e2, N) ->
            checkTypingD ctx e1 N
            && checkTypingD ctx e2 N
        _ -> False
    , case (e, t) of
        (Var x, _) -> checkLookup ctx x t
        _          -> False
    , case (e, t) of
        (Lam t1 e', Arr t1' t2) ->
            t1 == t1'
            && checkTypingD (t1 : ctx) e' t2
        _ -> False
    , case (e, t) of
        (App e1 e2, _) ->
            any (\t1 ->
              checkTypingD ctx e1 (Arr t1 t))
              (enumTyping ctx e2)
        _ -> False
    ]

-- ============================================================================
-- PART 3: Derived checker for indexed datatype
-- ============================================================================

enumExpr :: [Ty] -> Expr -> [Ty]
enumExpr = undefined

checkExpr :: [Ty] -> Ty -> Expr -> Bool
checkExpr _ N (ECon _) = True
checkExpr ctx N (EAdd e1 e2) =
    checkExpr ctx N e1
    && checkExpr ctx N e2
checkExpr ctx t (EVar x) =
    checkLookup ctx x t
checkExpr ctx (Arr t1 t2) (ELam t1' e) =
    t1 == t1'
    && checkExpr (t1 : ctx) t2 e
checkExpr ctx t (EApp e1 e2) =
    any (\t1 ->
      checkExpr ctx (Arr t1 t) e1)
      (enumExpr ctx e2)
checkExpr _ _ _ = False
