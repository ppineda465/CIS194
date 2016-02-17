------------exercise 1
--1.1
fun1 :: [Integer] -> Integer
fun1 = product . (map $ subtract 2) . filter even

--1.2
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile(/=1) . iterate l
  where l n=if even n then n`div` 2 else  (3*n-1)

------------exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree val Leaf        = Node 0 Leaf val Leaf
insertTree v (Node h l x r)
    | height l < height r = Node h (insertTree v l) x r
    | height l > height r = Node h l x (insertTree v r)
    | otherwise           = Node (1 + height iRight) l x iRight
        where
            height Leaf = -1
            height (Node ht _ _ _) = ht
            iRight = insertTree v r



------------exercise 3
--3.1
xor :: [Bool] -> Bool
xor = odd.foldr (\x n -> if x == True then n+1 else n)0

--3.2
--map :: (a -> b) -> [a] -> [b]
--map f = foldr (\x xs -> f x : xs) []
