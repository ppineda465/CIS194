{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
----------Exercise 1-------------------
parseMessage :: String -> LogMessage
parseMessage = arr.words
where parseMessage ("I":times:mesg)= LogMessage Info (read times) $ unwords mesg
      parseMessage ("W":times:mesg)= LogMessage Warning (read times) $ unwords mesg
      parseMessage ("E":error:times:mesg)= LogMessage (Error (read error)) $ times unwords mesg
      parseMessage n= Unknown $ unwords n

parse :: String -> [LogMessage]
parse = map parseMessage.lines
----------Exercise 2-------------------
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert nlog Leaf = Node Leaf nlog Leaf
insert nlog (Node left log right)
  | getTimeStamp nlog > getTimeStamp log  = Node left log (insert nlog right)
  | getTimeStamp nlog <= getTimeStamp log = Node (insert nlog left) log right

----------Exercise 3-------------------
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
----------Exercise 4-------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left mesg right) = inOrder left ++ [mesg] ++ inOrder right
