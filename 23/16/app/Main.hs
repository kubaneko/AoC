module Main where

import Data.Char
import Data.Tuple
import Data.Maybe
import Control.Monad
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Trans.State.Strict

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

h = True

solve i = if h then maximum$fmap reflect (fmap singleton entries) else reflect [((0,0),(0,1))]
  where
    entries :: [((Int,Int), (Int,Int))]
    entries = nub$do -- disgusting
      y <- [0..(V.length parsed) -1]
      x <- [0..(V.length$parsed ! 0) -1]
      [((y,0),(0,1)), ((y,(V.length$ parsed ! 0) -1),(0,-1)),
       ((0,x),(1,0)), (((V.length$parsed) -1,x),(-1,0))]
    reflect r = length$S.toList$ S.map fst $ execState (ezFun r)  S.empty
    parsed = V.fromList $ fmap V.fromList $ lines i
    ezFun [] = return ()
    ezFun (a:as) = do
            rs<-runRay a
            ezFun (rs++as)
    runRay a@(cor, dir) = do
      s <- get
      if a `S.member` s || index2D cor parsed == Nothing
        then return []
        else do
               put (S.insert a s)
               return $ fmap (\a-> (pairPl a cor,a))$concat$index2D cor parsed >>= (return.toDirs dir)
toDirs :: (Int,Int) -> Char -> [(Int,Int)]
toDirs (a,b) '/' = [(-b,-a)]
toDirs a '\\' = [swap a]
toDirs a '.' = [a]
toDirs a@(0,_) '-'=[a]
toDirs (_,0) '-'=[(0,1),(0,-1)]
toDirs a  '|'= fmap swap $ toDirs (swap a) '-'

pairPl (a,b) (c,d) = (a+c,b+d)

index2D (y,x) vv = vv !? y >>= (!? x)
