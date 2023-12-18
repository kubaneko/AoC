module Main where

import Data.Char
import Data.Maybe
import Control.Monad
import Data.Tuple.Extra (fst3, snd3)
import Data.List.Extra (groupOn)
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Safe as S
import qualified Safe.Exact as S
import Data.List.Split (splitOn)
import Control.Monad.Trans.State.Strict
import Data.List.Utils (dropWhileList)
import Algorithm.Search


h = False

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

solve i = (ez,hr)
  where
    ez = shoeLace' (reverse coords) + (sum$fmap snd$ filter filFun $fmap fst parsed) + 1
    hr = shoeLace' (reverse coords') + (sum$fmap snd$ filter filFun decoded) + 1
    parsed = fmap ((\case [dir, len, col] -> ((dir, read @Integer len),col)).words)$lines i
    decoded = fmap (\case ('(':'#':xs) -> (tDir$last$init xs, hexToInt$init$init xs))$fmap snd parsed
    coords = foldl' fFun [(0,0)] $fmap fst parsed
    coords' = foldl' fFun [(0,0)] decoded
    tDir '0' = "R"
    tDir '1' = "D"
    tDir '2' = "L"
    tDir '3' = "U"
    fFun xs@(co1:_) (dir, len) = (pairPl co1 (conv dir len)):xs
    filFun ("L",_) = True
    filFun ("D",_) = True
    filFun _ = False
    conv "L" n = (0,-n)
    conv "R" n = (0,n)
    conv "U" n = (-n,0)
    conv "D" n = (n,0)


-- last and first coor should be equal
shoeLace' [_] = 0
shoeLace' as@(a:_) = sum (withWindow 2 fFun as) `div` 2
  where
    fFun [(y1,x1),(y2,x2)] = (y1+y2)*(x1-x2)
shoeLace' [] = 0

withWindow :: Int -> ([a]->b) -> [a] -> [b]
withWindow n f xs@(_:rst) = case S.takeExactMay n xs of
                            Just ys -> f ys : withWindow n f rst
                            Nothing -> []
withWindow _ _ [] = []

pairPl (a,b) (c,d) = (a+c,b+d)

hexToInt :: String -> Int
hexToInt = foldl ((+) . (16 *)) 0 . map digitToInt
