maximum' :: (Ord a) => [a] -> a
maximum' []       = error "maximum of empty list"
maximum' [x]      = x
maximum' (x:tail)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' tail


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : (take' (n - 1) xs)

repeat' :: a -> [a]
repeat' x = x:repeat' x

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      largerSorted  = quickSort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ largerSorted
