import Data.List

main = do
  contents <- getContents
  let actions = words contents

  print $ calcScore $ zipWith (\x y -> (toAction x, toAction y)) [actions !! x | x <- [0,2..(length actions)]] [actions !! x | x <- [1,3..(length actions)]]

toAction :: String -> Action
toAction x 
  | x == "A" || x == "X" = Rock
  | x == "B" || x == "Y" = Paper
  | otherwise = Scissor

data Action = Rock | Paper | Scissor deriving (Eq, Show, Enum, Read)

getWin :: (Action , Action) -> Int
getWin (a, b)
  | a == b = 1
  | c /= [] = 2
  | otherwise = 0
  where c = filter (==(a, b))  [(Rock, Paper), (Paper, Scissor), (Scissor, Rock)]

calcScore :: [(Action, Action)] -> [Int]
calcScore xs = scanl (\x y -> (+x) (getWin y) * 3 + 1 + fromEnum (snd y)) 0 xs
  