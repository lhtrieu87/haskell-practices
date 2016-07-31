type Matrix a          = [Row a]
type Row a             = [a]
type Grid              = Matrix Digit
type Digit             = Int

digits :: [Int]
digits = [1 .. 9]

blank :: Digit -> Bool
blank = (== 0)

{-Fill empty cells to get all permutations, then filter only those valid-}
solve :: Grid -> [Grid]
solve = filter valid . completions

{-Find all possible choices for each blank cell, then expand to all permutations-}
completions :: Grid -> [Grid]
completions = expand . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice) 

choice :: Digit -> [Digit]
choice d = if blank d then digits else [d]

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp []                   = [[]]
cp (xs:xss)             = [x:ys | x <- xs, ys <- yss]
                          where yss = cp xss

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x] 
remove ds xs = filter (`notElem` ds) xs

valid :: Grid -> Bool
valid g = all nodups (rows  g) &&
          all nodups (cols  g) &&
          all nodups (boxes g)

nodups :: (Eq a) => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs]     = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxes :: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map cols . group . map group 

group :: [a] -> [[a]]
group [] = []
group xs = (take 3 xs):group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

