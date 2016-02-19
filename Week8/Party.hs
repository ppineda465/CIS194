module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List
import Data.Ord


-------exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e: employees gl) (fun gl + empFun e)
   where
        employees (GL emps _ )= emps
        fun (GL _ f )= f


instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) ( GL e2 f2 )= GL (e1 ++ e2)(f1 + f2)



moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2 = gl1
  | otherwise = gl2

-------exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))


-------exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (bestWith, bestWithout)
  where bestWith = glCons b $ mconcat $ map snd gls
        bestWithout = mconcat $ map (uncurry moreFun) gls

-- exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
