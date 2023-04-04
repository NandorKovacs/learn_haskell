split :: Char -> String -> [String]
split c [] = []
split c s = (next c s) : split c (drop ((length . next c) s + 1) s) 

next :: Char -> String -> String
next c [] = []
next c (s:sx) = if s == c then [] else s : (next c sx)