factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n -1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + y1, x2 + y2)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= fat = "Overweight"
  | otherwise                   = "Are you a human?"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat    = 30.0

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi weight height | (weight, height) <- xs]
  where bmi weight height = weight / height ^ 2

cylinderArea :: (RealFloat a) => a -> a -> a
cylinderArea r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [bmi | (weight, height) <- xs, let bmi = weight / height ^ 2]
