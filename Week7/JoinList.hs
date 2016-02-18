{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Buffer
import Data.Monoid
import Editor
import Scrabble
import Sized

--------EXERCISE 1
data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)


data JoinList m a  =  Empty
                      |Single m a
                      |Append m (JoinList m a) (JoinList ma)
   deriving (Eq,Show)


instance Monoid m => Monoid(JoinList m a ) where
  mempty= Empty
  mappend= (+++)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x mappend tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag Append (t _ _) = t

-----------EXERCISE 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ e) = Just e
indexJ i (Append _ l r) | d < 0     = indexJ i l
                        | otherwise = indexJ d r
                        where d = difJ i l
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = hJ (const Empty) id (+++)
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = hJ id (const Empty) (+++)

----------EXERCISE 3
scoreLine :: String -> JoinList Score String
scoreLine = listToJL scoreString . monkey
