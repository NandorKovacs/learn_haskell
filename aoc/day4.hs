main = do
  contents <- getContents
  print $ sumContainingPairs $ map lineToPairPair (lines contents)

lineToPairPair :: String -> ((Int, Int), (Int, Int))
lineToPairPair s = do 
  let func = (\x -> case x of '-' -> ' '
                              ',' -> ' '
                              otherwise -> x)
  wordsToPairPair $ words $ map func s

wordsToPairPair :: [String] -> ((Int, Int), (Int, Int))
wordsToPairPair (a:b:c:d:_) = ((read a, read b),(read c,read d))

doesAContainB :: (Int, Int) -> (Int, Int) -> Bool
doesAContainB a b = fst a <= fst b && snd a >= snd b

doRangesContain :: ((Int, Int), (Int, Int)) -> Bool
doRangesContain (a, b) = doesAContainB a b || doesAContainB b a

sumContainingPairs :: [((Int, Int), (Int, Int))] -> Int
sumContainingPairs [] = 0
sumContainingPairs (x:xs) = fromEnum (doRangesContain x) + sumContainingPairs xs