{-# language TupleSections #-}

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
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Safe as S
import qualified Safe.Exact as S

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

solve i = (solE,solH)
  where
    solE = (`div` 2)$(+1)$fst3$last$loop
    solH = fst3$foldl' (foldFun parsed) ((0,0), lastDir loop, vvMinus (fmap snd3 loop) $fmap (fmap Just) parsed )$ init$tail$fmap snd3 loop
    lastDir ((_,(a,b),_):(_,(c,d),_):_) = (c-a,d-b)
    loop = takeWhile (\case (d,_,lst) -> lst /= start || d==1)$iterate (itFun parsed) (0,start, (-1,-1))
    parsed = V.fromList$fmap (V.fromList.fmap toRep) $lines i
    start = head$catMaybes$V.toList$fmap (\case (_,Nothing) -> Nothing; (y, Just x) -> Just (y,x))$V.indexed
      $fmap (V.elemIndex allE) parsed

index2D (y,x) vv = vv !? y >>= (!? x)

component vv comp coor = if not (coor `elem` comp) && (isJust$join$index2D coor vv)
        then foldl' (component vv)  (coor:comp) allN' else comp
  where
    allN' = fmap (pairPl coor) allN

type MV = V.Vector (V.Vector (Maybe [(Int, Int)]))
type VV = V.Vector (V.Vector [(Int, Int)])

foldFun :: VV
                      -> ((Int, Int), (Int, Int), MV)
                      -> (Int, Int)
                      -> ((Int, Int), (Int, Int), MV)
foldFun vvo ((l,r), ldir, vv) coor = ((l+length lc, r + length rc), lDir, vv'')
  where
    (ls,rs) = leftRight ldir (fromJust $ index2D coor vvo) allN
    (lc,vv') = foldl' (\st l -> (fst st ++ component (snd st) [] l,vvMinus (component (snd st) [] l) (snd st))) ([],vv) (fmap (pairPl coor) ls)
    (rc,vv'') = traceShow (coor, (l,r), ldir, lc)$foldl' (\st r -> (fst st ++ component (snd st) [] r,vvMinus (component (snd st) [] r) (snd st))) ([],vv') (fmap (pairPl coor) rs)
    lDir = head$filter ((/=(0,0)).pairPl ldir) $fromJust $ index2D coor vvo

leftRight ldir divide coors = partition partF coors
  where
    partF coor = and$fmap (divCond ldir coor) divide
    divCond :: (Int,Int)->(Int,Int)->(Int,Int)->Bool
    divCond ld@(1,0) (y', x') coor@(y,x)
     | pairPl ld coor == (0,0) = True
     | x == 1 = y' < 0 && x' > 0
     | x == -1 = y' > 0 || x' > 0
     | x == 0 = x' > 0
    divCond (-1,0) coor@(y,x) (y', x') = divCond (1,0) (-y,-x) (-y',-x')
    divCond (y'',x'') coor@(y,x) (y', x') = divCond (- x'',y'') (- x,y) (- x',y')


vvMinus :: [(Int,Int)] -> MV -> MV
vvMinus coors vv = V.accum (\v xs -> v V.// fmap (,Nothing) xs) vv coors'
  where
    coors' = fmap (\xs -> (fst$head xs, fmap snd xs))$groupOn fst$sortOn fst coors


pairPl (a,b) (c,d) = (a+c,b+d)

toRep '|' = [(1,0), (-1,0)]
toRep '-' = [(0,1), (0,-1)]
toRep 'L' = [(-1,0), (0,1)]
toRep 'J' = [(-1,0), (0,-1)]
toRep 'F' = [(1,0), (0,1)]
toRep '7' = [(1,0), (0,-1)]
toRep '.' = []
toRep 'S' = allE

allE = [(1,0), (-1,0), (0,1), (0,-1)]
allN = tail$[ (y,x) | y <- [0,-1,1], x <- [0,-1,1]]

itFun parsed (dist,curr,lst) = (dist+1,next,curr)
    where
        next = fromJust$do
                dirs <- index2D curr parsed
                find (/=lst) (catMaybes$fmap (reached curr) dirs)
        reached coor dir = do
                dirs <- index2D (pairPl dir coor) parsed
                S.headMay$catMaybes$fmap (\dir2 -> if (==(0,0))$pairPl dir dir2 then Just (pairPl dir coor) else Nothing) dirs
