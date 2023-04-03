import Data.List

main = do
  contents <- getContents
  let a = biggestElf contents
  print $ sum $ drop (length a - 3) (sort a)

biggestElf :: String -> [Int]
biggestElf [] = []
biggestElf x = do
  let a = takeWhile (/="") (lines x)
  let b =  sum $ map read a
  b : biggestElf (drop (length a + 1) x)
