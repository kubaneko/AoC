module Main where

import qualified Data.Set as S
import GHC.Utils.Misc (split)
import Debug.Trace
import Data.List


main :: IO ()
main = do
  i <- readFile "input"
  print$sum$fmap (solveE.fmap (fmap (read @Int).words).split '|').lines$ i
  print$sum$solveH (repeat 1)$ fmap (fmap (fmap (read @Int).words).split '|').lines$ i

solveE (win:our:[]) = if m == 0 then 0 else 2^(m-1)
  where
    win' = drop 2 win
    m = length$intersect win' our

solveH (n:xs) ((a:b:[]):scs) = n:(solveH (zipWith (+) xs (replicate m n ++ repeat 0)) scs)
  where
    a' = drop 2 a
    m = length$intersect a' b
solveH _ [] = []
