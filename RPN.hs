solveRPN :: String -> Float
solveRPN = head . foldl step [] . words
  where step (x:y:ys) "+" = (x + y):ys
        step (x:y:ys) "-" = (y - x):ys
        step (x:y:ys) "*" = (x * y):ys
        step (x:y:ys) "/" = (y / x):ys
        step xs "sum"      = [sum xs]
        step xs c         = read c : xs
