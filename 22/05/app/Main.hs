{-# LANGUAGE TypeApplications #-}
import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Data.List.Tools
import           Debug.Trace
import           Distribution.Simple.Utils

traceI :: Show a => a -> a
traceI a = trace (show a) a

main :: IO ()
main = do
  file <- readFile "input"
  print $ calculate False (lines file)

type Easy = Bool -- True je Easy

calculate ez lns = foldl (\i -> brb i . safeHead) "" (foldl fld stck instrs)
 where
  brb i (Just b) = i ++ b
  brb i Nothing  = i
  stck :: [[String]]
  stck =
    parseStack (filter (liftA2 (&&) (/= Nothing) (/= Just 'm') . safeHead) lns)
  instrs :: [[Int]]
  instrs = parseInstr <$> filter ((== Just 'm') . safeHead) lns
  fld :: [[String]] -> [Int] -> [[String]]
  fld stk (x : y : z : _) = setAt
    (setAt stk (y - 1) (drop x from))
    (z - 1)
    ((if ez then reverse else id) load ++ stk !! (z - 1))
   where
    from = stk !! (y - 1)
    to   = stk !! (z - 1)
    load = take x from

parseInstr :: String -> [Int]
parseInstr instr = read <$> dropEven 0 (splitOn " " instr)

dropEven 0 (x : xs) = dropEven 1 xs
dropEven 1 (x : xs) = x : dropEven 0 xs
dropEven _ []       = []

parseStack lns = add
  frst
  ((fmap ugly . splitOn " " . subst <$> tail (reverse lns)) :: [[String]])
 where
  subst (' ' : ' ' : ' ' : ' ' : ys) = '.' : ' ' : subst ys
  subst (a   : b   : c   : ' ' : xs) = a : b : c : ' ' : subst xs
  subst (' '       : ' ' : ' ' : ys) = '.' : subst ys
  subst (a         : b   : c   : xs) = a : b : c : subst xs
  subst []                           = []
  frst = [] <$ filter (not . null) $ splitOn " " $ head $ reverse lns
  add :: [[String]] -> [[String]] -> [[String]]
  add empt = foldl add1 empt
  add1 (y : ys) ([] : xs) = y : add1 ys xs
  add1 (y : ys) (x  : xs) = (x : y) : add1 ys xs
  add1 _        []        = []
  ugly []         = []
  ugly ('.' : xs) = ugly xs
  ugly ('[' : xs) = ugly xs
  ugly (']' : xs) = ugly xs
  ugly (' ' : xs) = ugly xs
  ugly (x   : xs) = x : ugly xs

