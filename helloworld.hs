f :: (Int -> Int) -> Int -> (Int -> Int)
f a b = (\x -> x + a (2 * b) + 1)

