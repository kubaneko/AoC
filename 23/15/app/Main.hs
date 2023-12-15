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
  print $ solve i

solve i = (ez, h)
  where
    parsed = splitOn ","$init i
    ez = sum$fmap (hash 0) parsed
    h = sum$score$foldl' foldFun (V.fromList$replicate 256 []) parsed

foldFun :: V.Vector [(String, Int)] -> [Char] -> V.Vector [(String, Int)]
foldFun v i = if last i == '-' then
                        v V.// [(boxf,deleteBy (\a b->fst a == fst b) (parsedm,0) (v ! boxf))] else
                        v V.// [(boxe,brrr (v ! boxe))]
  where
    [l,f] = splitOn "=" i
    boxe = hash 0 l
    parsedm = init i
    boxf = hash 0 parsedm
    brrr ((l1,f1):xs)
      | l1 == l = (l,read @Int f):xs
      | otherwise = (l1,f1):brrr xs
    brrr [] = [(l,read @Int f)]

score v = V.imap (\i k -> sum$V.toList$V.imap (\j -> \case (l,f) -> (i+1)*(j+1)*f) (V.fromList k) ) v

hash n (x:xs) = hash (((n+ord x)*17) `mod` 256) xs
hash n [] = n
