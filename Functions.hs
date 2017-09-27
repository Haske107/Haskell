
module Functions where

-- PROBLEM 1 : COUNT
count :: String -> Char -> Int
count m c = length $ filter (== c) m

-- PROBLEM 2 : REVERSE
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- PROBLEM 3 : SORT
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) =
  let smallerSorted = sort [ a | a <- xs, a <= x]
      biggerSorted = sort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- POLYNOMIAL TIME
type Polynomial = [Double]

-- PROBLEM 4A: EVALUATION POLYNOMIALS AT A POINT

-- PROBLEM 4B: ADDING POLYNOMIALS
addPoly :: Polynomial -> Polynomial-> Polynomial
addPoly [] y = y
addPoly x [] = x
addPoly (x:xs) (y:ys) = x + y : addPoly xs ys

-- PROBLEM 4C: SCALE POLYNOMIALS
scale :: Double -> Polynomial -> Polynomial
scale s [] = []
scale s (x:xs) = [s * x] ++ scale s xs

-- PROBLEM 4D: MULTIPLYING POLYNOMIAL
multiPoly :: Polynomial -> Polynomial -> Polynomial
multiPoly a [] = []
multiPoly [] a = []
multiPoly (x:xs) a = addPoly (scale x a) (0: (multiPoly xs a))

-- PROBLEM 4E: TAKING DERIVATIVES
derivative :: Polynomial -> Polynomial
derivative [a] = []
derivative a = derivative(take (length a - 1) a) ++  [(last a * (length a - 1) )]

-- PROBLEM 4F: TAKING INTEGRALS
-- integral :: Polynomial -> Polynomial
-- integral [] = []1
-- integral (x:xs) = unzip2 xs [1..]

-- PROBLEM 5: FACTREC
