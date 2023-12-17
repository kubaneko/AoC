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
import Algorithm.Search

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

h = True

type Dir = (Int,Int)
type Coor = (Int,Int)
data GState = GState Dir Int Int Coor
  deriving (Eq,Ord,Show)
solve i = fst$fromJust$dijkstra (neighbours parsed (if h then 10 else 3) (if h then 4 else 1)) (cost parsed) (end parsed) (GState (0,0) 0 0 (0,0))
  where
    parsed = V.fromList$fmap (V.fromList.fmap (read @Int .singleton))$lines i


neighbours vv mm ll (GState dir@(a,b) most least cr) = catMaybes$fmap fFun allE
  where
    fFun d
     | isNothing$index2D (pairPl d cr) vv = Nothing
     | d == dir && most == mm = Nothing
     | d == dir = Just (GState d (most+1) (least-1) (pairPl d cr))
     | d == (-a,-b) = Nothing
     | least > 0 = Nothing
     | otherwise = Just (GState d 1 (ll-1) (pairPl d cr))

end vv (GState _ _ l (a,b)) = if a == length vv -1 && b == length (vv ! 0) -1 && (not h || l <= 0) then True else False

cost vv (GState _ _ _ _) (GState _ _ _ cr) = fromJust$index2D cr vv

index2D (y,x) vv = vv !? y >>= (!? x)
allE = [(1,0), (-1,0), (0,1), (0,-1)]
pairPl (a,b) (c,d) = (a+c,b+d)
