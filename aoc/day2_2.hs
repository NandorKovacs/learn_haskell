import Data.List

main = do
  contents <- getContents
  let actions = words contents

  print $ calcScore $ zipWith (\x y -> (toAction x, toOutcome y)) [actions !! x | x <- [0,2..(length actions)]] [actions !! x | x <- [1,3..(length actions)]]

toAction :: String -> Action
toAction x 
  | x == "A" || x == "X" = Rock
  | x == "B" || x == "Y" = Paper
  | otherwise = Scissor

toOutcome :: String -> Outcome
toOutcome x 
  | x == "X" = Lose
  | x == "Y" = Tie
  | otherwise = Win

data Outcome = Lose | Tie | Win deriving (Eq, Show, Enum, Read)
data Action = Rock | Paper | Scissor deriving (Eq, Show, Enum, Read)

outcomeToAction :: (Action, Outcome) -> Action
outcomeToAction (a, b)
  | b == Tie = a
  | a == Rock = if b == Win then Paper else Scissor
  | a == Paper = if b == Win then Scissor else Rock
  | otherwise = if b == Win then Rock else Paper

calcScore :: [(Action, Outcome)] -> Int
calcScore [] = 0
calcScore (x:xs) = fromEnum (snd x) * 3 + 1 + fromEnum (outcomeToAction x) + calcScore xs
  