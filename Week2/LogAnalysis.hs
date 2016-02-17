{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
----------Exercise 1-------------------
parseMessage :: String -> LogMessage
parseMessage a = case words a of
          ("I":times:mesg) -> LogMessage Info (read times) $ unwords mesg
          ("W":times:mesg) -> LogMessage Warning (read times) $ unwords mesg
          ("E":errors:times:mesg) -> LogMessage (Error (read errors)) (read times) $ unwords mesg
          _ -> Unknown a

parse :: String -> [LogMessage]
parse = map parseMessage.lines
----------Exercise 2-------------------
insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(LogMessage _ ts _) (Node left lm'@(LogMessage _ ts' _) right)
    | ts < ts'  = Node (insert lm left) lm' right
    | otherwise = Node left lm' (insert lm right)

insert lm Leaf  = Node Leaf lm Leaf
insert _  mt    = mt

----------Exercise 3-------------------
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
----------Exercise 4-------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left mesg right) = inOrder left ++ [mesg] ++ inOrder right
