
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-------exercise 1
fib :: Integer -> Integer
fib n |n<=1 = n
      |otherwise = fib (n-1)+fib (n-2)

fibs1 :: [Integer]
fibs1=map fib[0..]
-------exercise 2
fib' :: Integer -> Integer
fib' n |n<=1 = n
      |otherwise = fib (n-1)+fib (n-2)

fibs1' :: [Integer]
fibs1'=map fib[0..]
-------exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-------exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x(streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))
