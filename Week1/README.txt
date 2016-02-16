--To apply a function to some arguments, just list the arguments after the function, separated by spaces, like this:

f :: Int -> Int -> Int -> Int
                           ^
The last Int               | its what the function returns

--Patern Matching
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs
