idx c s = [i | i <- [0..(length s - 1)], s !! i == c]

getrange l a b = take (b-a) (drop a l)

split c s = if null (idx c s) then [s] else ([getrange s 0 (head (idx c s))] ++ [getrange s ((idx c s) !! (i - 1) + 1) ((idx c s) !! i) | i <- [1..(length (idx c s) - 1)]] ++ [getrange s (last (idx c s) + 1) (length s)])

