module Main where

import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Safe as S

main :: IO ()
main = do
  i <- readFile "input"
  print$solveE i

solveE i = S.elemIndexJust "ZZZ"$fmap fst$ iterate foldFun ("AAA", parsedI)
  where
    lns = lines i
    parsedI = cycle$head lns
    parsedNds = M.fromList$fmap ((\case [a,b,c] -> (a,(b,c))).words)$tail lns
    foldFun (str,i:is) = case (parsedNds M.! str, i) of
      ((_,r),'R') -> (r,is)
      ((l,_),'L') -> (l,is)

