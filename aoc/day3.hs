import Data.List
import Data.Char

main = do
  contents <- getContents
  let rucksacks = lines contents
  print $ rucksackSum rucksacks

rucksackSum :: [String] -> Int
rucksackSum [] = 0
rucksackSum (x:xs) = (letterToInt . getCommon) (take (length x `div` 2) x, drop (length x `div` 2) x) + rucksackSum xs

letterToInt :: Char -> Int
letterToInt x = if isUpper x then ord x - ord 'A' + 27 else ord x - ord 'a' + 1

getCommon :: (String, String) -> Char
getCommon (a, b) = getDuplicate (nub a ++ nub b)

getDuplicate :: String -> Char
getDuplicate [] = '!'
getDuplicate (x:xs) = if elem x xs then x else getDuplicate xs