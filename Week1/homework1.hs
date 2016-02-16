{-# OPTIONS_GHC -Wall #-}
module homework1 where

-- Exercise 1 -----------------------------------------

toDigits :: Integer -> [Integer]
toDigits n =  reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n<=0 = []
toDigitsRev n = n `mod` 10:toDigitsRev(n `div` 10)


-- Exercise 2 -----------------------------------------
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []=[]
doubleEveryOther [x] = [x]
doubleEveryOther [x,y]= [x*2,y]
doubleEveryOther l@(x:y:xs)
        | length l `mod` 2==0 = x*2:y:doubleEveryOther xs
        |otherwise = x:y*2:doubleEveryOther xs


-- Exercise 3 -----------------------------------------

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)=sum(toDigits x)+sumDigits xs


-- Exercise 4 -----------------------------------------

validate :: Integer -> Bool
validate n
  | sumDigits(doubleEveryOther(toDigitsRev n)) `mod` 10==0 = True
  |otherwise = False



-- Exercise 5 -----------------------------------------


type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a c b
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a b c ++
      ((a, c) : hanoi (n - 1) b c a)
