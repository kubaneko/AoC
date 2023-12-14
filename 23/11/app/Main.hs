{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Debug.Trace

main :: IO ()
main = do
  i <- readFile "input"
  print $ solve i

h = True

solve i = dist 0 parsed2
  where
    parsed = expand2D $ V.toList
        $ fmap (\case (y, v) -> V.toList $ fmap (\case (x, '#') -> (True, (y, x)); _ -> (False, (-1, -1))) $ V.indexed $ V.fromList v)
        $ V.indexed $ V.fromList $ lines i
    parsed2 = fmap snd . filter (/= (False, (-1, -1))) $ concat parsed

expand2D ls = transpose $ fmap (fmap (\case (b, k) -> (b, swap k))) $ expand 0
  $ fmap (fmap (\case (b, k) -> (b, swap k))) $ transpose e1
  where
    e1 = expand 0 ls

expand k (l1 : ls) = if not $ any fst l1 then expand (k + (if h then 999999 else 1)) ls
  else fmap (\case (True, (y, x)) -> (True, (y + k, x)); k -> k) l1 : expand k ls
expand _ [] = []

dist d l@((y, x) : xs) = dist ((d +) $ sum $ fmap (\case (y', x') -> abs (y - y') + abs (x - x')) xs) xs
dist d [] = d
