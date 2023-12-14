{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Vector ((!),(!?), imap)
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import qualified GHC.List as G
import Data.Tuple.Extra (fst3)
import Data.Bifunctor (bimap)

data GearsEz = No | MaybePart Char | Part Char
  deriving (Show)

data GearsH = NoH | MI Char | GI Char [(Int,Int)]
  deriving (Show)

main :: IO ()
main = do
  i <- readFile "input"
  print$ solveE (fmap V.fromList$V.fromList$lines i)
  print$ solveH (fmap V.fromList$V.fromList$lines i)

solveE vv = V.foldl' (\i v -> i + fst3 (flip foldFun No$V.foldl' foldFun (0,[],False) v)) 0 $
            imap (\i -> imap (\j -> check i j vv)) vv
  where
    foldFun (k,l,False) No = (k,[],False)
    foldFun (k,l,True) No = (k + read @Int (reverse l),[],False)
    foldFun (k,l,b) (MaybePart p) = (k, p:l, b)
    foldFun (k,l,b) (Part p) = (k, p:l, True)

check y x vv curr
  | isDigit curr = if checkSurr y x vv then Part curr else MaybePart curr
  | otherwise = No

checkSurr y x vv = G.or$mapMaybe (\case (x,y) -> vv !? y >>= (!? x) >>= (\c -> return$not (isDigit c) && c /= '.')) [(x+z, y+w) | w <- [-1..1], z <- [-1..1] ]

solveH vv = foldl' (\i->(i+).fst) 0.filter (\x->snd x == (2 :: Int)).fmap snd.M.toList$V.foldl' (\i v -> fst3$flip foldFun NoH$V.foldl' foldFun (i,[],[]) v) M.empty $
            imap (\i -> imap (\j -> checkH i j vv)) vv
  where
    checkH y x vv curr
      | isDigit curr = if not$null$checkSurrH y x vv  then GI curr (checkSurrH y x vv) else MI curr
      | otherwise = NoH
    foldFun (m,l,[]) NoH = (m,[],[])
    foldFun (m,l,g) NoH = (foldl' (\mp coor -> M.insertWith (\case (a,b) -> \case (c,d) -> (a*c,b+d)) coor (read @Int$reverse l,1) mp) m g ,[],[])
    foldFun (m,l,g) (MI p) = (m, p:l, g)
    foldFun (m,l,g) (GI p gs) = (m, p:l, nub$gs ++ g)


checkSurrH y x vv = mapMaybe (\case (x,y) -> vv !? y >>= (!? x) >>= (\c -> if c =='*' then Just (x,y) else Nothing)) [(x+z, y+w) | w <- [-1..1], z <- [-1..1] ]
