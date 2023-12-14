module Main where

import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Safe as S
import qualified Safe.Exact as S

main :: IO ()
main = do
  i <- readFile "input"
  print$solveE i

hard = True

solveE :: String -> Int
solveE i = sum$fmap (foldl' (if hard then (\b a -> head a - b) else (\b a -> last a + b)) 0.
                     reverse.takeWhile (not.and.fmap (==0)).iterate derive) parsed
  where
    parsed = fmap (fmap (read @Int).words)$lines i
    derive xs
      | and (fmap (==0) xs) = xs
      | otherwise =  withWindow 2 (\case [x,y] -> y-x) xs


withWindow n f xs@(_:rst) = case S.takeExactMay n xs of
                            Just ys -> f ys : withWindow n f rst
                            Nothing -> []
withWindow _ _ [] = []
