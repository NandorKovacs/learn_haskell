main = do
  contents <- getContents

biggestElf :: [String] -> Int
biggestElf x = do
  let a = takeWhile (/="") x
  let b =  sum $ map read::Int a
  let c = biggestElf drop (length a) x
  if c > b then c else b