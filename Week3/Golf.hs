{-# OPTIONS_GHC -Wall #-}

module Golf where

-----exercise 1
skips :: [a] -> [[a]]
skips xs = map (select xs) [1.. (length xs)]
  where
    select :: [a]-> Int -> [a]
    select xs n = map fst $ filter (\x -> (snd x) `mod` n==0)(zip xs [1..])


------exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:l@(y:z:xs))
  | y > x && y >z = y : (localMaxima l)
  |otherwise = localMaxima l
localMaxima _ =[]

------exercise 3
--histogram :: [Integer] -> Strings
