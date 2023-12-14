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

h = True

main :: IO ()
main = do
  i <- readFile "input"
  print$solve i

solve i = (sum$fmap ((100*).palindromes) parsed) + (sum$fmap (palindromes.transpose) parsed)
  where
    parsed = splitOn [""] $lines i

palindrome [] = True
palindrome (l1:l2:ls) = if l1 == (last (l2:ls)) then palindrome (init (l2:ls)) else False
palindrome _ = False

palindrome' True [] = True
palindrome' False [] = False
palindrome' True (l1:l2:ls) = case (l1, last (l2:ls)) of
  ([],[]) -> palindrome' True $init (l2:ls)
  ((ll1:lls1), (ll2:lls2)) -> if ll1 == ll2 then palindrome' True (lls1:(init (l2:ls) ++ [lls2])) else False
palindrome' False (l1:l2:ls) = case (l1, last (l2:ls)) of
  ([],[]) -> palindrome' False $init (l2:ls)
  ((ll1:lls1), (ll2:lls2)) -> if ll1 == ll2 then palindrome' False (lls1:(init (l2:ls) ++ [lls2])) else palindrome' True (lls1:(init (l2:ls) ++ [lls2]))
palindrome' _ _ = False

palindromes m = case (m1,m2) of
  ([],[]) -> 0
  ([],k) -> (length k `div` 2)
  (k,[]) -> length m - length k + (length k `div` 2)
  where
    cond = if h then not.(palindrome' False) else not.palindrome
    m1 = dropWhileList cond m
    m2 = dropWhileList cond (reverse m)
