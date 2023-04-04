import Data.List
import Data.Char

main = do
  contents <- getContents
  let rucksacks = lines contents
  print $ rucksackSum rucksacks

rucksackSum :: [String] -> Int
rucksackSum [] = 0
rucksackSum (x:xs) = (ord . getCommon) (take (length x / 2) x, drop (length x / 2) x) - ord 'a' + rucksackSum xs

getCommon :: (String, String) -> Char
getCommon (a, b) = getDuplicate (nub a ++ nub b)

getDuplicate :: String -> Char
getDuplicate [] = '!'
getDuplicate (x:xs) = if elem x xs then x else getDuplicate xs