main = do
  contents <- getContents
  let l = lines contents
  let crates = getCrates l
  let actions = getActions l
  print $ topCrates crates actions

getCrates :: [String] -> [[Char]]
getCrates ((' ':'1':x):_) = take (length x + 3) (repeat [])
getCrates (x:xs) = lineToCrate x `stack` getCrates xs

lineToCrate :: String -> [Char]
lineToCrate [] = []
lineToCrate ('[':x:']':xs) = x : (lineToCrate $ drop 4 xs)
lineToCrate x = lineToCrate $ drop 4 x 

stack :: [Char] -> [[Char]] -> [[Char]]
stack [] [] = []
stack (x:xs) (y:ys) = (x:y) : xs `stack` ys

getActions :: [String] -> [Action]
getActions [] = []
getActions (x@('m':'o':'r':'e':_):xs) = do
  let w = words x
  Action {amount = read (head (drop 1 w)), from = read (head (drop 3 w)), to = read (head (drop 5 w))} : getActions xs
getActions (x:xs) = getActions xs

topCrates :: [[Char]] -> [Action] -> [Char]
topCrates a b = [head y | y <- performActions a b]

data Action = Action {amount::Int, from::Int, to::Int}

performActions :: [[Char]] -> [Action] -> [[Char]]
performActions a [] = a 
performActions a (b:bs) = do
  let l = length a
  let func i = if i == from b then take (l - amount b) (a !! i) else if i == to b then (a !! i) ++ take (l - amount b) (a !! from b) else (a !! i) 
  performActions [func i |i <- [0..l]] bs