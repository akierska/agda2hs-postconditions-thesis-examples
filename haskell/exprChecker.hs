type Fuel = Int

enumExprTy :: Fuel -> Fuel -> [Ty] -> Expr -> [Maybe Ty]
enumExprTy = undefined

checkExpr :: Fuel -> Fuel -> [Ty] -> Ty -> Expr -> Maybe Bool
checkExpr 0 _ ctx ty e = backtrack
    [ case (e, ty) of
        (ECon _, N) -> Just True
        _           -> Nothing
    , case e of
        EVar x -> checkLookup ctx x ty
        _      -> Nothing
    ]
checkExpr fuel topFuel ctx ty e = backtrack
    [ case (e, ty) of
        (ECon _, N) -> Just True
        _           -> Just False
    , case (e, ty) of
        (EAdd l r, N) ->
            checkExpr (fuel - 1) topFuel ctx N l
            `andM`
            checkExpr (fuel - 1) topFuel ctx N r
        _ -> Just False
    , case e of
        EVar x -> checkLookup ctx x ty
        _      -> Just False
    , case (e, ty) of
        (ELam t1 body, Arr t1' t2) ->
            Just (t1 == t1')
            `andM`
            checkExpr (fuel - 1) topFuel (t1 : ctx) t2 body
        _ -> Just False
    , case e of
        EApp e1 e2 ->
            anyM (\mt1 -> case mt1 of
                    Just t1 -> checkExpr (fuel - 1) topFuel ctx (Arr t1 ty) e1
                    Nothing -> Nothing)
                 (enumExprTy (fuel - 1) topFuel ctx e2)
        _ -> Just False
    ]
