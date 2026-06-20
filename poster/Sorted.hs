checkSorted :: [Int] -> Bool
checkSorted = or 
    [ case arr of 
        [] -> True 
        _  -> False
    , case arr of 
        (x : []) -> True 
        _        -> False 
    , case arr of 
        (x : y : xs) -> x <= y && checkSorted (y : xs) 
        _            -> False 
    ]