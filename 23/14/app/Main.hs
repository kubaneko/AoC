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
import GHC.Utils.Misc (nTimes)

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

data Rocks = RoundR | Empty | SquareR | StuckR
  deriving (Show, Eq)

withWindow :: Int -> ([a]->b) -> [a] -> [b]
withWindow n f xs@(_:rst) = case S.takeExactMay n xs of
                            Just ys -> f ys : withWindow n f rst
                            Nothing -> []
withWindow _ _ [] = []

solve i = (sum$fmap (score 1.rollRocks) (rotate parsed), sum$fmap (score 1)$traceShowId$rotate$fst$head$dropWhile (\a->fst a /= snd a)$withWindow 2 (\case [a,b]->(a,b))$iterate' (traceShowId.oneCycle) parsed)
  where
    parsed = fmap (fmap (\case '.' -> Empty; '#' -> SquareR; 'O' -> RoundR))$lines i
            --
          --

rotate = fmap reverse.transpose

rollRocks (RoundR:RoundR:xs) = rollRocks$RoundR:rollRocks (RoundR:xs)
rollRocks (RoundR:Empty:xs) = Empty:rollRocks (RoundR:xs)
rollRocks (RoundR:SquareR:xs) = StuckR:SquareR:rollRocks xs
rollRocks (RoundR:StuckR:xs) = StuckR:StuckR:xs
rollRocks [RoundR] = [StuckR]
rollRocks (x:xs) = x:rollRocks xs
rollRocks [] = []

oneCycle = head.drop 4.iterate (fmap rollRocks.rotate.fmap unstick)

score k (StuckR:xs) = k + score (k+1) xs
score k (_:xs) = score (k+1) xs
score k [] = 0

withF :: (a->a) -> (a->a) -> a -> a
withF g f = g.f.g

unstick = fmap (\case StuckR -> RoundR; a -> a)
