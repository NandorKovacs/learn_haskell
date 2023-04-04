import Data.List
import Data.Char

main = do
  contents <- getContents
  let rucksacks = lines contents
  print $ rucksackSum $ zip3 [rucksacks !! x | x <- [0, 3..length rucksacks]] [rucksacks !! x | x <- [1, 4..length rucksacks]] [rucksacks !! x | x <- [2, 5..length rucksacks]]

rucksackSum :: [(String, String, String)] -> Int
rucksackSum [] = 0
rucksackSum (x:xs) = (letterToInt . getCommon) x + rucksackSum xs

letterToInt :: Char -> Int
letterToInt x = if isUpper x then ord x - ord 'A' + 27 else ord x - ord 'a' + 1

getCommon :: (String, String, String) -> Char
getCommon (a, b, c) = getDuplicate (nub a ++ nub b ++ nub c)

getDuplicate :: String -> Char
getDuplicate [] = '!'
getDuplicate (x:xs) = if (length . filter (==x)) xs > 1 then x else getDuplicate xs