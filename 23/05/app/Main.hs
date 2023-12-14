module Main where

import qualified Data.Set as S
import Debug.Trace
import Data.List
import Data.List.Split (split, oneOf)


data Range = Range {
  cFrom :: Integer,
  dFrom :: Integer,
  rSize :: Integer
             }

main :: IO ()
main = do
  i <- readFile "input"
  (sH,s,mps) <- parse i
  print$ solveE (s,mps)
  print$ solveE (sH,mps)

parse i = return (seedsH seeds, seeds, fmap (sortOn dFrom.fmap (\case (d:c:s:[]) -> Range d c s)) rest)
  where
    seeds = fmap (read @Integer)$drop 1$words$head$head prepar
    seedsH (s:n:xs) = [s..(s+n-1)] ++ (seedsH xs)
    seedsH [] = []
    rest :: [[[Integer]]]
    rest = fmap (fmap (fmap (read @Integer).words).drop 1)$drop 1$ prepar
    prepar = split (oneOf [""]) $ lines i

solveE (sd, mps) = minimum$fmap (\s -> foldl' foldFun s mps ) sd
  where
    foldFun a b = (\case {[] -> a; ((Range c d r):_) -> if d + r - 1 < a then a else c + (a - d)})$reverse$takeWhile (\case (Range _ d _) -> d <= a) b
