{-# language FlexibleContexts #-}

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

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

h = True

type Cache = M.Map (Maybe Char, Int, Bool, Int, Int) Integer

solve :: String -> Integer
solve i = if h then sum$fmap prepare (fmap (unfoldI 5) parsed) else sum$fmap prepare parsed
  where
    parsed = fmap ((\case [mpg,intvs] -> (mpg, fmap (read @Int)$splitOn "," intvs)).words)$lines i
    prepare = (\case p@(sm,is) -> evalState (numComb 0 False p (sum is) (length$filter (=='#') sm) (length$filter (/='.') sm)) M.empty)

unfoldI 1 p = p
unfoldI k (syms, intvs) = (syms ++ ('?':symsk), intvs ++ intvsk)
  where
    (symsk,intvsk) = unfoldI (k-1) (syms, intvs)

numCombChk :: Int -> Bool -> ([Char], [Int]) -> Int -> Int -> Int -> State Cache Integer
numCombChk rIntvs endI (sm, iss) intSum restHash restPossHash
  | restHash > intSum = return 0
  | intSum > restPossHash = return 0
  | otherwise = do
      m <- get
      case M.lookup (listToMaybe sm,rIntvs, endI, length sm, length iss) m of
        Just i -> return i
        Nothing -> do
                   i <- numComb rIntvs endI (sm, iss) intSum restHash restPossHash
                   modify' (M.insert (listToMaybe sm,rIntvs, endI, length sm, length iss) i)
                   return i


numComb :: Int -> Bool -> ([Char], [Int]) -> Int -> Int -> Int -> State Cache Integer
numComb 0 endI ([], []) _ _ _ = return 1
numComb 0 False (('.':sm), iss) intSum restHash restPossHash = numCombChk 0 False (sm, iss) intSum restHash restPossHash
numComb 0 False (('#':sm), iss@(i:is)) intSum restHash restPossHash = numCombChk (i-1) (checkInt i) (sm, is) (intSum-1) (restHash-1) (restPossHash-1)
numComb 0 False (('#':_), []) _ _ _ = return 0
numComb 0 False (('?':sm), iss) intSum restHash restPossHash = liftM2 (+) (numCombChk 0 False (('.':sm), iss) intSum restHash (restPossHash -1)) (numCombChk 0 False (('#':sm), iss) intSum (restHash +1) restPossHash)
numComb rIntvs endI ((s:sm), iss) intSum restHash restPossHash
  | rIntvs > 0 && (s == '?') = numCombChk (rIntvs - 1) (checkInt rIntvs) (sm, iss) (intSum-1) restHash (restPossHash-1)
  | rIntvs > 0 && (s == '#') = numCombChk (rIntvs - 1) (checkInt rIntvs) (sm, iss) (intSum-1) (restHash-1) (restPossHash-1)
  | endI && (s == '.') = numCombChk rIntvs False (sm, iss) intSum restHash restPossHash
  | endI && (s == '?') = numCombChk rIntvs False (sm, iss) intSum restHash (restPossHash-1)
  | otherwise = return 0
numComb _ _ _ _ _ _ = return 0


checkInt 1 = True
checkInt _ = False
