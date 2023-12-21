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

main :: IO ()
main = do
  i <- readFile "input"
  print$solveH i

data Part = Part Integer Integer Integer Integer
data Rl = Cond Char Char Integer String | Jump String
  deriving (Show)

solve i = foldl'
  (\n b -> n+
    ((\case "A" -> (\case Part a b c d -> a + b + c + d) b; _ -> 0)$
     fst$head$dropWhile (\case (i,_) -> i /= "A" && i /= "R") $iterate (evalR b) ("in", parsedR M.! "in"))) 0 parsedP
  where
    evalR :: Part -> (String,[Rl]) -> (String, [Rl])
    evalR p (_, Jump nm:rls) = (nm, parsedR M.! nm)
    evalR p (nm, Cond at op n jmp :rls) = if holds p at op n then (jmp, parsedR M.! jmp) else evalR p (nm, rls)
    [rules,parts] = splitOn [""]$lines i
    parsedP = fmap ((\case [x,m,a,s] -> Part x m a s).fmap (read @Integer .drop 2).splitOn ",".tail.init) parts
    parsedR = M.fromList$fmap toR rules

holds (Part x m a s) 'x' = comp x
holds (Part x m a s) 'm' = comp m
holds (Part x m a s) 'a' = comp a
holds (Part x m a s) 's' = comp s
comp n1 '<' n2 = n1 < n2
comp n1 '>' n2 = n1 > n2

toR r = (nm, pRls)
  where
    (nm, rls) = break (=='{') r
    pRls = fmap pR (splitOn ","$tail$init rls)

pR str@(i:x:s)
  | x == '<' || x == '>' = case splitOn ":" s of [n,jmp] -> Cond i x (read @Integer n) jmp
  | otherwise = Jump str
pR [a] = Jump (singleton a)

data Interval=Interval (Integer,Integer) (Integer,Integer) (Integer,Integer) (Integer,Integer)
  deriving (Show)

solveH i = numberValid (Interval (1,4000) (1,4000) (1,4000) (1,4000)) "in" (parsedR M.! "in")
  where
    [rules,parts] = splitOn [""]$lines i
    parsedR = M.fromList$([("A",[]), ("R",[])]++) $ fmap toR rules
    numberValid intv@(Interval (a,b) (c,d) (e,f) (g,h)) "A" _ = traceShow intv$traceShowId$(b-a+1)*(d-c+1)*(f-e+1)*(h-g+1)
    numberValid _ "R" _ = 0
    numberValid intv nm (Jump s:xs) = numberValid intv s (parsedR M.! s)
    numberValid intv nm rls = sum$fmap (setup rls) [True,False]
      where
        setup (Cond i op n jmp:_) True = numberValid (updateIntv i (newIntv op n) intv) jmp (parsedR M.! jmp)
        setup (Cond i '<' n jmp:xs) False = numberValid (updateIntv i (newIntv '>' (n-1)) intv) nm xs
        setup (Cond i '>' n jmp:xs) False = numberValid (updateIntv i (newIntv '<' (n+1)) intv) nm xs



newIntv '<' n (a,b) = if n > a && a > 0 then (a,min (n-1) b)  else (-1,-2)
newIntv '>' n (a,b) = if n < b && b > 0 then (max a (n+1), b) else (-1,-2)

updateIntv 'x' f (Interval x m a s) = Interval (f x) m a s
updateIntv 'm' f (Interval x m a s) = Interval x (f m) a s
updateIntv 'a' f (Interval x m a s) = Interval x m (f a) s
updateIntv 's' f (Interval x m a s) = Interval x m a (f s)
